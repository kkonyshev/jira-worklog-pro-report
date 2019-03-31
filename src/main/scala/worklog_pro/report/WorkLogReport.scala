package worklog_pro.report

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.typesafe.config.{Config, ConfigFactory}
import org.joda.time.DateTime
import play.api.libs.json._
import play.api.libs.ws.{StandaloneWSClient, WSAuthScheme}
import play.api.libs.ws.ahc.StandaloneAhcWSClient

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object WorkLogReport extends App {
  assert(args.length>=2, "dateStart/dateEnd must be provided (actual: " + args + "), ex: WorklogReport \"2019-03-01\" \"2019-03-01\"")

  val start = DateTime.parse(args(0))
  val end = DateTime.parse(args(1))

  val workLogReportGenerator = new WorklogReportGenerator(new AppConfig(ConfigFactory.defaultApplication()))

  println(s"Generating workLog report from ${start.toString(YYYY_MM_DD_FORMAT)} till ${end.toString(YYYY_MM_DD_FORMAT)}")
  println("===================")

  val timeSheet: TimeSheetResult = workLogReportGenerator.generate(start, end)

  val required = timeSheet.totalHoursRequired()
  println(s"Total working hour/days required: $required / ${required/8}")
  val logged = timeSheet.totalHoursLogged()
  println(s"Logged working hours/days: $logged / ${logged/8}")

  println()
  println(s"Daily report")
  println("===================")
  println("Date;LoggedHours;RequiredHours;L/R")
  timeSheet.dailyReport.map(r => s"${r.date.toString(YYYY_MM_DD_FORMAT)};${r.loggerHours};${r.requiredHours};${r.loggedOverRequiredHours}").foreach(println)

  System.exit(0)
}

class AppConfig(config: Config) {
  val username = config.getString("api.username")
  val password = config.getString("api.password")
  val baseUrl = config.getString("api.url")
}

class WorklogReportGenerator(appConfig: AppConfig) {

  import play.api.libs.ws.JsonBodyReadables._
  import play.api.libs.ws.JsonBodyWritables._
  import play.api.libs.functional.syntax._
  import WorklogReportGeneratorFormats._

  // Create Akka system for thread and streaming management
  implicit val system = ActorSystem()
  system.registerOnTermination {
    System.exit(0)
  }
  implicit val materializer = ActorMaterializer()

  def timeSheet(wsClient: StandaloneWSClient, start: DateTime, end: DateTime): Future[TimeSheet] = {
    val serviceUrl = s"${appConfig.baseUrl}timesheet/user?startDate=${start.toString(YYYY_MM_DD_FORMAT)}&endDate=${end.toString(YYYY_MM_DD_FORMAT)}&targetKey=${appConfig.username}"
    wsClient.url(serviceUrl)
      .withAuth(appConfig.username, appConfig.password, WSAuthScheme.BASIC)
      .addHttpHeaders("Content-Type" -> "application/json", "Accept" -> "application/json")
      .get().map { response ⇒ response.body[JsValue].as[TimeSheet] }
  }

  def workHoursSchema(wsClient: StandaloneWSClient, start: DateTime, end: DateTime): Future[WorkHoursSchema] = {
    val serviceUrl = s"${appConfig.baseUrl}workHoursSchema/workHours?startDate=${start.toString(YYYY_MM_DD_FORMAT)}&endDate=${end.toString(YYYY_MM_DD_FORMAT)}&excludeHolidays=false&userKeys=${appConfig.username}"
    wsClient.url(serviceUrl)
      .withAuth(appConfig.username, appConfig.password, WSAuthScheme.BASIC)
      .addHttpHeaders("Content-Type" -> "application/json", "Accept" -> "application/json")
      .get().map { response =>
      response.body[JsValue].as[WorkHoursSchema](WorkHoursSchemaReads(appConfig.username))
    }
  }

  def generate(start: DateTime, end: DateTime): TimeSheetResult = {
    val wsClient = StandaloneAhcWSClient()
    for {
      w <- workHoursSchema(wsClient, start, end)
      t <- timeSheet(wsClient, start, end)
    } yield {
      TimeSheetResult(t, w, start, end)
    }
  }.awaitResult
}

object WorklogReportGeneratorFormats {

  import play.api.libs.ws.JsonBodyReadables._
  import play.api.libs.ws.JsonBodyWritables._
  import play.api.libs.functional.syntax._

  implicit val WorkLogFormat: OFormat[WorkLog] = Json.format[WorkLog]
  implicit val IssueFormat: OFormat[Issue] = Json.format[Issue]
  implicit val ProjectFormat: OFormat[Project] = Json.format[Project]
  implicit val TimesheetFormat: OFormat[TimeSheet] = Json.format[TimeSheet]

  //https://www.lagomframework.com/documentation/1.4.x/scala/Serialization.html
  case class WorkHoursSchemaReads(username: String) extends Reads[WorkHoursSchema] {
    def reads(json: JsValue) = {
      val workingHours = (JsPath \ username \ "workHours").read[List[Double]].reads(json)
      JsSuccess(WorkHoursSchema(username, workingHours.get))
    }
  }
}
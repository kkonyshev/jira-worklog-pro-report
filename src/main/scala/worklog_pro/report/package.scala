package worklog_pro

import java.util.concurrent.TimeUnit

import org.joda.time.DateTime

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

package object report {

  val YYYY_MM_DD_FORMAT = "yyyy-MM-dd"

  implicit class DatabaseOperationResult [T](future: Future[T]) {
    def awaitResult = Await.result(future, 10.seconds)
  }

  /*
    Jira api response model
     */

  case class TimeSheet(isWeekend: List[Boolean], daysBetween: Int, projects: List[Project])

  case class Project(key: String, issues: List[Issue])

  case class Issue(key: String, workLogs: List[WorkLog])

  //timeSpent sec
  case class WorkLog(timeSpent: Long, workStart: Long) {
    def getTimeSpent() = Duration(timeSpent, TimeUnit.SECONDS)
  }


  /*
  Internal model
   */
  case class WorkHoursSchema(username: String, hours: Seq[Double])

  case class TimeSheetResult(timeSheet: TimeSheet, workHoursSchema: WorkHoursSchema, start: DateTime, end: DateTime) {
    def fromToString = {
      s"from ${start.toString(YYYY_MM_DD_FORMAT)} till ${start.toString(YYYY_MM_DD_FORMAT)}"
    }

    def getDaysUntil(tillDate: Option[DateTime] = None) = dailyReport.filter(w => tillDate.map(rd => w.date.isBefore(rd.withMillisOfSecond(0))).getOrElse(true))

    def totalHoursRequired(tillDate: Option[DateTime] = None): Double = getDaysUntil(tillDate).map(_.requiredHours).sum
    def totalHoursLogged(tillDate: Option[DateTime] = None): Double = getDaysUntil(tillDate).map(_.loggerHours).sum

    def dailyReport: Seq[DayReport] = {
      val workHoursZipped : Map[Int, Double] = workHoursSchema.hours.zipWithIndex.map(g => g._2 -> g._1).toMap

      val reported: Map[DateTime, Double] = timeSheet.projects.flatMap(_.issues).flatMap(_.workLogs).map(wl => {
        new DateTime(wl.workStart).withMillisOfDay(0) -> wl.timeSpent.toDouble
      }).groupBy(_._1).map( g => (g._1, (g._2.map(_._2).sum / 60 / 60).toDouble)).toList.sortBy(_._1.getMillis).toMap

      for {
        dayShift <- 0 to timeSheet.daysBetween
        requiredHours <- workHoursZipped.get(dayShift)
        d = start.plusDays(dayShift)
      } yield {
        reported.get(d) match {
          case Some(reportedHours) => DayReport(d, requiredHours, reportedHours)
          case None => DayReport(d, requiredHours, 0.0)
        }

      }
    }
  }

  case class DayReport(date: DateTime, loggerHours: Double, requiredHours: Double) {
    def loggedDays: Double = loggerHours / 8.0
    def requiredDays: Double = requiredHours / 8.0
    def loggedOverRequiredHours: Double = loggerHours / requiredHours
    def loggedOverRequiredDays: Double = loggedDays / requiredDays
    override def toString: String = s"DayReport(onDate=${date.toString(YYYY_MM_DD_FORMAT)}, required=$requiredHours, logged=$loggerHours)"
  }
}

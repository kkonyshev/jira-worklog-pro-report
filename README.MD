# Report generator for [Worklog PRO jira plugin](https://denizoguz.atlassian.net/projects/WLP)

- set username and password in application.conf
- build
- get port: `./bin/jira-worklog-pro-report "2019-03-01" "2019-03-31"`

## Output example:

```
Generating workLog report from 2019-03-01 till 2019-03-31
===================
Total working hour/days required: 168.0 / 21.0
Logged working hours/days: 168.0 / 21.0

Daily report
===================
Date;LoggedHours;RequiredHours;L/R
2019-03-01;8.0;8.0;1.0
2019-03-02;0.0;0.0;NaN
2019-03-03;0.0;0.0;NaN
2019-03-04;8.0;8.0;1.0
2019-03-05;8.0;8.0;1.0
2019-03-06;8.0;8.0;1.0
2019-03-07;8.0;8.0;1.0
2019-03-08;8.0;8.0;1.0
2019-03-09;0.0;0.0;NaN
2019-03-10;0.0;0.0;NaN
2019-03-11;8.0;8.0;1.0
2019-03-12;8.0;8.0;1.0
2019-03-13;8.0;8.0;1.0
2019-03-14;8.0;8.0;1.0
2019-03-15;8.0;8.0;1.0
2019-03-16;0.0;0.0;NaN
2019-03-17;0.0;0.0;NaN
2019-03-18;8.0;8.0;1.0
2019-03-19;8.0;8.0;1.0
2019-03-20;8.0;8.0;1.0
2019-03-21;8.0;8.0;1.0
2019-03-22;8.0;8.0;1.0
2019-03-23;0.0;8.0;0.0
2019-03-24;0.0;0.0;NaN
2019-03-25;8.0;8.0;1.0
2019-03-26;8.0;8.0;1.0
2019-03-27;8.0;8.0;1.0
2019-03-28;8.0;8.0;1.0
2019-03-29;8.0;0.0;Infinity
2019-03-30;0.0;0.0;NaN
2019-03-31;0.0;0.0;NaN
```
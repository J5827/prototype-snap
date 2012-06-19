------------------------------------------------------------------------------
-- TABLE STRUCTURE                                                          --
------------------------------------------------------------------------------

drop table IF EXISTS tutor;
create table tutor (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  firstname,
  lastname,
  school
);

drop table IF EXISTS student;
create table student (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  firstname,
  lastname,
  school
);

drop table IF EXISTS course;
create table course (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  tutorId INTEGER,
  name,
  semester,
  assessment,
  starttime,
  enrollment
);

drop table IF EXISTS coursegroup;
create table coursegroup (
  id INTEGER PRIMARY KEY,
  courseId INTEGER,
  tutorId INTEGER,
  name,
  capacity INTEGER
);

drop table IF EXISTS task;
create table task (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  tutorId INTEGER,
  name,
  status,
  scoring,
  config
);

drop table IF EXISTS solution;
create table solution (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  studentId INTEGER,
  taskId INTEGER,
  content,
  score INTEGER
);

drop table IF EXISTS enrollment;
create table enrollment (
  studentId INTEGER,
  coursegroupId INTEGER,
  time,
  PRIMARY KEY (studentId, coursegroupId)
);

drop table IF EXISTS assignment;
create table assignment (
  taskId INTEGER,
  courseId INTEGER,
  starttime,
  deadline,
  PRIMARY KEY (taskId, courseId)
);

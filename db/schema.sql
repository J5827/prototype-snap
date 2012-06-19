------------------------------------------------------------------------------
-- TABLE STRUCTURE                                                          --
------------------------------------------------------------------------------

drop table IF EXISTS tutor;
create table tutor (
  id        INTEGER PRIMARY KEY,
  firstname TEXT,
  lastname  TEXT,
  school    TEXT
);

drop table IF EXISTS student;
create table student (
  id        INTEGER PRIMARY KEY,
  firstname TEXT,
  lastname  TEXT,
  school    TEXT
);

drop table IF EXISTS course;
create table course (
  id         SERIAL PRIMARY KEY,
  tutorId    INTEGER,
  name       TEXT,
  semester   TEXT,
  assessment TEXT,
  starttime  TEXT,
  enrollment TEXT
);

drop table IF EXISTS coursegroup;
create table coursegroup (
  id       SERIAL PRIMARY KEY,
  courseId INTEGER,
  tutorId  INTEGER,
  name     TEXT,
  capacity INTEGER
);

drop table IF EXISTS task;
create table task (
  id      SERIAL PRIMARY KEY,
  tutorId INTEGER,
  name    TEXT,
  status  TEXT,
  scoring TEXT,
  config  TEXT
);

drop table IF EXISTS solution;
create table solution (
  id        SERIAL PRIMARY KEY,
  studentId INTEGER,
  taskId    INTEGER,
  content   TEXT,
  score     INTEGER
);

drop table IF EXISTS enrollment;
create table enrollment (
  studentId     INTEGER,
  coursegroupId INTEGER,
  time          TEXT,
  PRIMARY KEY (studentId, coursegroupId)
);

drop table IF EXISTS assignment;
create table assignment (
  taskId    INTEGER,
  courseId  INTEGER,
  starttime TEXT,
  deadline  TEXT,
  PRIMARY KEY (taskId, courseId)
);

------------------------------------------------------------------------------
-- TABLE STRUCTURE                                                          --
------------------------------------------------------------------------------

drop table if exists users;
create table users (
  id           SERIAL PRIMARY KEY,
  auth_user_id INTEGER,
  firstname    TEXT,
  lastname     TEXT,
  email        TEXT,
  school       TEXT,
  role         TEXT
);

drop table if exists courses;
create table courses (
  id         SERIAL PRIMARY KEY,
  user_id    INTEGER,
  name       TEXT,
  semester   TEXT,
  assessment TEXT,
  starttime  TEXT,
  enrollment TEXT
);

drop table if exists coursegroups;
create table coursegroups (
  id        SERIAL PRIMARY KEY,
  course_id INTEGER,
  user_id   INTEGER,
  name      TEXT,
  capacity  INTEGER
);

drop table if exists tasks;
create table tasks (
  id      SERIAL PRIMARY KEY,
  user_id INTEGER,
  name    TEXT,
  status  TEXT,
  scoring TEXT,
  config  TEXT
);

drop table if exists solutions;
create table solutions (
  id      SERIAL PRIMARY KEY,
  user_id INTEGER,
  taskId  INTEGER,
  content TEXT,
  score   INTEGER,
  time    TEXT
);

drop table if exists enrollments;
create table enrollments (
  user_id        INTEGER,
  coursegroup_id INTEGER,
  time           TEXT,
  PRIMARY KEY (user_id, coursegroup_id)
);

drop table if exists assignments;
create table assignments (
  task_id   INTEGER,
  course_id INTEGER,
  starttime TEXT,
  deadline  TEXT,
  PRIMARY KEY (task_id, course_id)
);

<apply template="default">

<h1>Autotool Übersicht</h1>

<div>
  <h2>Eingeschriebene Kurse</h2>
  <div>
    <myCourseList>
      <div>
        <h3><myCourseName /></h3>
        <table border="1">
          <thead>
            <tr>
              <td>Aufgabe</td>
              <td>Status</td>
              <td>Bearbeitungszeit</td>
              <td>gelöst</td>
              <td>Highscore</td>
              <td>Details</td>
            </tr>
          </thead>
          <tbody>
            <taskList>
              <tr>
                <td><taskName /></td>
                <td><taskStatus /></td>
                <td><taskDeadline /></td>
                <td><taskSolved /></td>
                <td>
                  <a href="/highscore/${taskId}"><taskHighscore /></a>
                </td>
                <td>
                  <a href="/task/solve/${taskId}">Link</a>
                </td>
              </tr>
            </taskList>
          </tbody>
        </table>
      </div>
    </myCourseList>
  </div>
</div>
              
<div>
  <h2>verfügbare Kurse</h2>
  <ul>
    <enrolCourseList>
      <li>
        <a href="/course/enrol/${enrolCourseId}"><enrolCourseName /></a>
      </li>
    </enrolCourseList>
  </ul>
</div>

</apply>

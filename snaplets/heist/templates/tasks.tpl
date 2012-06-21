<apply template="default">

  <div class="well">
    <h3>currently available autotool tasks</h3>
    <br />

    <bind tag="category">
      <span class="category"><categoryName /></span>
      <ul>
        <subTrees>
          <li><element /></li>
        </subTrees>
      </ul>
    </bind>

    <bind tag="task">
      <a href="/task/${taskName}"><taskName /></a>
    </bind>

    <ul id="task-types">
      <taskTrees>
        <li><element /></li>
      </taskTrees>
    </ul>

  </div>

</apply>

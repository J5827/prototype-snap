<apply template="default">

  <h2>Aufgabe <task-name /></h2>
  <br />

  <h3>Hinweise</h3>
  <div class="well">
    Noch keine Hinweise.
  </div>

  <h3>Aufgabenstellung</h3>
  <task-description />

  <h3>Neue Einsendung</h3>
  <textarea id="task-example">
    <task-example />
  </textarea>

  <button class="btn" type="submit">
    Beispiel laden
  </button>
  <button class="btn" type="submit">
    Letzte Einsendung laden
  </button>
  <button class="btn btn-primary" type="submit">
    Lösung absenden
  </button>

  <br />
  <br />
  <h3>Weitere Dokumentation</h3>
  <div class="well">
    <task-documentation />
  </div>

</apply>

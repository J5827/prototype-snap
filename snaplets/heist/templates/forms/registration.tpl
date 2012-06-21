<apply template="default">

  <dfForm id="registration-form">

    <h2>Account anlegen</h2>

    <br />

    <dfChildErrorList class="alert alert-error" ref="" />

    <dfLabel class="control-label" ref="school">Schule</dfLabel>
    <dfInputSelect class="span3" ref="school" />

    <dfLabel class="control-label" ref="username">Matrikelnummer</dfLabel>
    <dfInputText class="span3" ref="username" />

    <dfLabel class="control-label" ref="firstname">Vorname</dfLabel>
    <dfInputText class="span3" ref="firstname" />

    <dfLabel class="control-label" ref="lastname">Nachname</dfLabel>
    <dfInputText class="span3" ref="lastname" />

    <dfLabel class="control-label" ref="email">E-Mail</dfLabel>
    <dfInputText class="span3" ref="email" />

    <br />

    <dfInputSubmit class="btn" value="Absenden" />

  </dfForm>

</apply>

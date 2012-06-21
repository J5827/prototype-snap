<apply template="default">

  <dfForm id="login-form">

    <h2>Login</h2>

    <br />
  
    <dfChildErrorList class="alert alert-error" ref="" />

    <dfLabel class="control-label" ref="school">Schule</dfLabel>
    <dfInputSelect class="span3" ref="school" />

    <dfLabel class="control-label" ref="username">Matrikelnummer</dfLabel>
    <dfInputText class="span3" ref="username" />

    <dfLabel class="control-label" ref="password">Passwort</dfLabel>
    <dfInputPassword class="span3" ref="password" />

    <dfLabel class="control-label" ref="remember">Eingeloggt bleiben</dfLabel>
    <dfInputCheckbox ref="remember" style="float: left; margin-right: 5px" />

    <br />
    <br />

    <dfInputSubmit class="btn" value="Login" />

  </dfForm>

  <br />

  <p>
    <a href="/register/">neuen Account anlegen</a>
  </p>

</apply>

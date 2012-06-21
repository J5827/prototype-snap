<apply template="default">

  <div class="hero-unit">
    <h1>Autotool'</h1>
    <p>An autotool client prototype written in Haskell.</p>
    <p><a class="btn btn-primary btn-large" href="/login">Login »</a></p>
  </div>

  <bind tag="dfa-url">http://dfa.imn.htwk-leipzig.de</bind>
  <bind tag="tool-url">https://autotool.imn.htwk-leipzig.de</bind>

  <footer>

    &copy; 2012 |

    <a href="${dfa-url}/auto/"
       target="_blank">Autotool</a> |

    autolat protocol v<protocol-version /> |

    autotool server v<server-version /> |

    <a href="${tool-url}/high/score.html"
       target="_blank">Highscores</a> |

    <a href="${dfa-url}/bugzilla/buglist.cgi?component=autotool"
       target="_blank">Bugs</a> |

    <a href="http://www.htwk-leipzig.de"
       target="_blank">HTWK Leipzig</a> |

    powered by
    <a href="http://snapframework.com"
       target="_blank">Snap</a>

  </footer>

</apply>

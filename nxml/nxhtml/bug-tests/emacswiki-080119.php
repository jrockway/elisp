<?php

  /* There was a problem reported 2008-01-19 on
     http://www.emacswiki.org/cgi-bin/wiki/NxhtmlMode. The word
     INCLUDE was highlighted with a different color. This is solved
     below:
   */

define("APP_INCLUDE", "/home/app/include/"); include_once(APP_INCLUDE . "file.php");

?>

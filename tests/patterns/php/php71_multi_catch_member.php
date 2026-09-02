<?php

// a multi-catch catches each of its types, so a rule naming any of them finds
// the clause, whatever order they are written in

//ERROR:
try { a(); } catch (First | Second $e) { log($e); }

//ERROR: the same two types, written the other way round
try { b(); } catch (Second | First $e) { log($e); }

//ERROR: among types the rule does not name
try { c(); } catch (Other | Second | First $e) { log($e); }

//ERROR: not the first catch of the try
try { d(); } catch (Other $e) {} catch (Second | First $e) { log($e); }

// OK: catches only one of the two
try { e2(); } catch (First $e) { log($e); }

// OK: catches neither
try { f(); } catch (Other | Another $e) { log($e); }

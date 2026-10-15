<?php

// the label of a heredoc may be double-quoted, which means the same as no
// quotes: the body is interpolated

//MATCH:
query(<<<"SQL"
SELECT * FROM t WHERE id = $id
SQL);

//MATCH: the same, unquoted
query(<<<SQL
SELECT * FROM t WHERE id = $id
SQL);

function f($id)
{
    //MATCH: with an indented closing label
    return query(<<<"SQL"
        SELECT * FROM t WHERE id = {$id}
        SQL);
}

//OK: a different function
other(<<<"SQL"
SELECT * FROM t WHERE id = $id
SQL);

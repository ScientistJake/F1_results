rm ergast.sqlite ;
sh convertdb.sh f1db_ansi.sql | sqlite3 ergast.sqlite

CREATE TABLE attendees
    ( id INTEGER PRIMARY KEY
    , name TEXT NOT NULL
    , comment TEXT NOT NULL
    );

CREATE INDEX attendees_index ON attendees (id);

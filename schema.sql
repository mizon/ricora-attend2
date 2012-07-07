CREATE TABLE attendees
    ( id INTEGER PRIMARY KEY
    , name TEXT NOT NULL
    , comment TEXT NOT NULL
    , encrypted_password TEXT NOT NULL
    );

CREATE INDEX attendees_index ON attendees (id);

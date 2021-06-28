CREATE TABLE benchmarks (
       id INTEGER PRIMARY KEY,
       name TEXT NOT NULL
);

CREATE INDEX benchmarks_name_idx ON benchmarks (name ASC);

CREATE TABLE runs (
       id INTEGER PRIMARY KEY,
       benchmark_id INTEGER NOT NULL,
       start_time INTEGER NOT NULL,
       end_time INTEGER NOT NULL,
       -- FIXME: create a tags table
       tag TEXT NOT NULL,
       user_run_time_us INTEGER,
       bytes_consed INTEGER
);

CREATE INDEX runs_benchmark_id_idx ON runs (benchmark_id ASC);

CREATE INDEX runs_start_time_idx ON runs (start_time ASC);

CREATE INDEX runs_end_time_idx ON runs (end_time ASC);

CREATE INDEX runs_tag_idx ON runs (tag ASC);

CREATE INDEX runs_user_run_time_us_idx ON runs (user_run_time_us ASC);

CREATE INDEX runs_bytes_consed_idx ON runs (bytes_consed ASC);

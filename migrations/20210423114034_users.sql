CREATE TABLE IF NOT EXISTS users (
	user_id uuid PRIMARY KEY,
	username TEXT NOT NULL,
  email TEXT NOT NULL UNIQUE,
	display_name TEXT NOT NULL,
	password TEXT NOT NULL,
	created_at TIMESTAMPTZ NOT NULL,
	updated_at TIMESTAMPTZ NOT NULL
);

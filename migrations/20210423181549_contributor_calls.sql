CREATE TABLE IF NOT EXISTS contributor_calls (
    contributor_call_id uuid NOT NULL,
    repository_id UUID NOT NULL,
    title TEXT NOT NULL,
    description TEXT NOT NULL,
	created_at TIMESTAMPTZ NOT NULL,
	updated_at TIMESTAMPTZ NOT NULL,
    CONSTRAINT contributor_calls_fk0 FOREIGN KEY ("repository_id")
        REFERENCES "repositories"("repository_id")
);

CREATE TABLE IF NOT EXISTS repositories (
    repository_id uuid PRIMARY KEY,
    organisation_id uuid NOT NULL,
    repository_name TEXT NOT NULL,
    repository_description TEXT NOT NULL,
    repository_url TEXT NOT NULL,
    repository_homepage TEXT,
	created_at TIMESTAMPTZ NOT NULL,
	updated_at TIMESTAMPTZ NOT NULL,
    CONSTRAINT repositories_fk0 FOREIGN KEY ("organisation_id")
        REFERENCES "organisations"("organisation_id")
);

CREATE INDEX repository_name_index ON repositories(repository_name);
CREATE UNIQUE INDEX repo_name_org ON repositories (repository_name, organisation_id);

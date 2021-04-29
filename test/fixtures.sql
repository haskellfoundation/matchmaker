-- You can load this file into the database by running:
-- $ psql "$PG_URI" < test/fixtures.sql
INSERT INTO "organisations" ("organisation_id",
                             "organisation_name",
                             "created_at",
                             "updated_at")
    VALUES ('b63ad088-a474-11eb-9236-5405db82c3cd',
            'ghchq',
            '2021-04-10 01:00:00Z',
            '2021-04-11 01:00:00Z'
);

INSERT INTO "users" ("user_id",
                     "username",
                     "email",
                     "display_name",
                     "password",
                     "created_at",
                     "updated_at")
    VALUES ('44495a98-a475-11eb-94f3-5405db82c3cd',
            'blue_devil',
            'princess_jack@example.com',
            'Princess Jack Moonshine',
            'DRINK!',
            '2021-04-23 14:00:00Z',
            '2021-04-23 14:30:00Z'
);

INSERT INTO "user_organisation" ("user_organisation_id",
                                 "user_id",
                                 "organisation_id",
                                 "is_admin")
    VALUES ('c798acb4-3446-48c2-a8ec-08799535c1e6',
            '44495a98-a475-11eb-94f3-5405db82c3cd',
            'b63ad088-a474-11eb-9236-5405db82c3cd',
            false
);

UPDATE "user_organisation" SET ("user_id",
                                "organisation_id",
                                "is_admin") =
                            ROW('44495a98-a475-11eb-94f3-5405db82c3cd',
                                'b63ad088-a474-11eb-9236-5405db82c3cd',
                                true)
    WHERE "user_organisation_id" = 'c798acb4-3446-48c2-a8ec-08799535c1e6';

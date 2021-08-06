{-# LANGUAGE BlockArguments #-}

module Integrations.GitHub
  ( listRepositories
  , githubRepoToRepository
  )
where

import DB.Organisation (OrganisationId (..))
import DB.Repository (Repository (..), RepositoryId (..))
import Data.Vector (Vector)

import qualified Data.Time as Time
import qualified Data.Time.Calendar.OrdinalDate as Time (fromOrdinalDate)
import qualified Data.UUID as Uuid
import qualified GitHub


listRepositories
  :: MonadIO m
  => Text
  -> m (Either GitHub.Error (Vector Repository))
listRepositories owner = liftIO do
  reposResult <-
    GitHub.github' GitHub.userReposR
      (GitHub.mkName (Proxy @GitHub.Owner) owner)
      GitHub.RepoPublicityOwner
      GitHub.FetchAll

  case reposResult of
    Left err -> pure $ Left err
    Right repos -> pure $ Right (fmap githubRepoToRepository repos)


githubRepoToRepository :: GitHub.Repo -> Repository
githubRepoToRepository repo = Repository
  { repositoryId = RepositoryId Uuid.nil
  , organisationId = OrganisationId Uuid.nil
  , repositoryName = GitHub.untagName (GitHub.repoName repo)
  , repositoryDescription = fromMaybe "" (GitHub.repoDescription repo)
  , repositoryURL = GitHub.getUrl (GitHub.repoUrl repo)
  , repositoryHomepage = GitHub.repoHomepage repo
  , createdAt = fromMaybe epoch (GitHub.repoCreatedAt repo)
  , updatedAt = fromMaybe epoch (GitHub.repoUpdatedAt repo)
  }
  where
  epoch = Time.UTCTime (Time.fromOrdinalDate 1970 0) 0

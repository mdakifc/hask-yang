module Grammar where

import Data.ByteString.Char8 as ByteString

-- Prefixing record members with 'y' so that we don't pollute a scope
-- with names that could be reused

type Id = ByteString
type Date = ByteString
type Yang = [YangModule]

-- YANG Module:
--  A YANG module contains a sequence of statements.  Each statement
--  starts with a keyword, followed by zero or one argument, followed
--  either by a semicolon (";") or a block of substatements enclosed
--  within braces ("{ }"):
--    statement = keyword [argument] (";" / "{" *statement "}")
--
--  [Q]
--    Can we we define a meta class that constraints statement type
--    constructors for follow the above rule?
--  [BEHAVIOR]
--    A module can define a complete, cohesive model,
--    or augment an existing data model with additional nodes.
--  [BEHAVIOR]
--    Submodules are partial modules that contribute definitions to a
--    module. A module may include any number of submodules, but each
--    submodule may belong to only one module.
--      submodule -> module
--      module -> [submodule]

data YangModule =
    YangModule
      { yModuleName :: Id
      , yModuleHeader :: YangModHeader
      , yModuleLinkage :: [ YangModLinkage ]
      }
  deriving (Eq, Show)

-- The "include" statement is used to make content from a submodule
-- available to that submodule's parent module, or to another submodule
-- of that parent module.  The argument is an identifier that is the
-- name of the submodule to include.
data YangModLinkage =
    YangImport
      { yImportId :: Id
      , yImportPrefix :: Maybe Id
      , yImportRevisionDate :: Maybe Date
      }
  | YangInclude
      { yIncludeId :: Id
      , yIncludeRevisionDate :: Maybe Date
      }
  deriving (Eq, Show)

data YangModHeader =
    YangModHeader
      { yYangVersion :: Maybe ByteString
      , yNamespace :: Id
      , yModPrefix :: Id
      }
  deriving (Eq, Show)

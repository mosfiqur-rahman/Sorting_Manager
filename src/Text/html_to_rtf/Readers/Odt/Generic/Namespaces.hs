

module Text.Pandoc.Readers.Odt.Generic.Namespaces where

import qualified Data.Map as M

--
type NameSpaceIRI          = String

--
type NameSpaceIRIs     nsID = M.Map nsID NameSpaceIRI

--
class (Eq nsID, Ord nsID) => NameSpaceID nsID where

  -- | Given a IRI, possibly update the map and return the id of the namespace.
  -- May fail if the namespace is unknown and the application does not
  -- allow unknown namespaces.
  getNamespaceID   :: NameSpaceIRI
                      -> NameSpaceIRIs nsID
                      -> Maybe (NameSpaceIRIs nsID, nsID)
  -- | Given a namespace id, lookup its IRI. May be overriden for performance.
  getIRI           :: nsID
                      -> NameSpaceIRIs nsID
                      -> Maybe NameSpaceIRI
  -- | The root element of an XML document has a namespace, too, and the
  -- "XML.Light-parser" is eager to remove the corresponding namespace
  -- attribute.
  -- As a result, at least this root namespace must be provided.
  getInitialIRImap :: NameSpaceIRIs nsID

  getIRI           = M.lookup
  getInitialIRImap = M.empty

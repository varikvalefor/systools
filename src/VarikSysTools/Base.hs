-- | Module:    VarikSysTools.Base
-- Decription:  le jicmu me'oi .type. poi se pilno le me'oi .library.
--              / Basic types which are used within this library
-- Copyright:   (c) Varik Valefor 2022
-- License:     Unlicense
-- Maintainer:  varikvalefor@aol.com
-- Stability:   experimental
-- Portability: portable
--
-- = la .lojban.
--
-- ni'o le ti me'oi .module. cu vasru le jicmu me'oi .type. poi se pilno
-- le me'oi .module. poi na du le ti me'oi .module. gi'e se vasru le ti
-- me'oi .library.
--
-- = English
--
-- This module contains the basic types which are used by the modules
-- which are not this module and are contained within this library.
module VarikSysTools.Base where

-- | = la .lojban.
--
-- ni'o ro da poi me'oi .'ErrorCode'. zo'u da skicu lo nu fliba
--
-- = English
--
-- For all 'ErrorCode' @t@, @t@ describes some sort of error.
type ErrorCode = String;

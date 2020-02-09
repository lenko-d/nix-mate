{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import           Control.Monad
import           Data.Maybe               (fromJust)
import           Data.Monoid              ((<>))
import           Data.Text                (Text, pack, unpack)
import           Foreign.Ptr              (castPtr)

import           Data.GI.Base
import qualified GI.Gio                   as Gio
import qualified GI.Gtk                   as Gtk
import           GI.Gtk.Objects.Container

import           System.Process

type ActionEntryInfo = (Text, Gio.ActionEntryActivateFieldCallback_WithClosures)

docEntryInfos :: [ActionEntryInfo]
docEntryInfos = [ ("list_installed_packages", actionActivated)
                , ("view_the_set_of_available_packages_in_nixpkgs", actionActivated)
                , ("upgrade_all-packages_for_which_there_are_newer_versions_dry_run", actionActivated)
                , ("upgrade_all-packages_for_which_there_are_newer_versions", actionActivated)
                , ("see_the_status_of_available_packages", actionActivated)
                , ("list_all_generations", actionActivated)
                , ("undo_nix_env_operation", actionActivated)
                , ("delete_all_old_gens_curr_profile", actionActivated)
                , ("run_gc", actionActivated)
                , ("run_gc_dry_run", actionActivated)
                , ("delete_old_gens_of_all_profiles", actionActivated)
                , ("obtain_latest_nix_expressions", actionActivated)
                , ("nix_version", actionActivated)
                , ("show_nix_config", actionActivated)
                , ("nixos_version", actionActivated)
                , ("upgrade_nix", actionActivated)
                ]




menuUIStr :: Text
menuUIStr = "<interface>\
            \  <menu id='doc-menu'>\
            \    <section>\
            \      <item>\
            \        <attribute name='label'>_List installed packages (nix-env -q)</attribute>\
            \        <attribute name='action'>list_installed_packages</attribute>\
            \      </item>\
            \      <item>\
            \        <attribute name='label'>_Upgrade all packages for which there are newer versions, dry run (nix-env -u --dry-run)</attribute>\
            \        <attribute name='action'>upgrade_all-packages_for_which_there_are_newer_versions_dry_run</attribute>\
            \      </item>\
            \      <item>\
            \        <attribute name='label'>_Upgrade all packages for which there are newer versions (nix-env -u)</attribute>\
            \        <attribute name='action'>upgrade_all-packages_for_which_there_are_newer_versions</attribute>\
            \      </item>\
            \      <item>\
            \        <attribute name='label'>_Obtain the latest Nix expressions available in a channel (nix-channel --update)</attribute>\
            \        <attribute name='action'>obtain_latest_nix_expressions</attribute>\
            \      </item>\
            \      <item>\
            \        <attribute name='label'>_Undo a nix-env operation (nix-env --rollback)</attribute>\
            \        <attribute name='action'>undo_nix_env_operation</attribute>\
            \      </item>\
            \      <item>\
            \        <attribute name='label'>_List all available generations (nix-env --list-generations)</attribute>\
            \        <attribute name='action'>list_all_generations</attribute>\
            \      </item>\
            \      <item>\
            \        <attribute name='label'>_Delete all old (non-current) generations in current profile (nix-env --delete-generations old)</attribute>\
            \        <attribute name='action'>delete_all_old_gens_curr_profile</attribute>\
            \      </item>\
            \      <item>\
            \        <attribute name='label'>_Run the garbage collector (dry run)  (nix-store --gc --print-dead)</attribute>\
            \        <attribute name='action'>run_gc_dry_run</attribute>\
            \      </item>\
            \      <item>\
            \        <attribute name='label'>_Run the garbage collector (nix-store --gc)</attribute>\
            \        <attribute name='action'>run_gc</attribute>\
            \      </item>\
            \      <item>\
            \        <attribute name='label'>_Delete all old generations of all profiles (quick and easy way to clean up the system)  (nix-collect-garbage -d)</attribute>\
            \        <attribute name='action'>delete_old_gens_of_all_profiles</attribute>\
            \      </item>\
            \      <item>\
            \        <attribute name='label'>_Nix version (nix --version)</attribute>\
            \        <attribute name='action'>nix_version</attribute>\
            \      </item>\
            \      <item>\
            \        <attribute name='label'>_NixOS version (nixos-version)</attribute>\
            \        <attribute name='action'>nixos_version</attribute>\
            \      </item>\
            \      <item>\
            \        <attribute name='label'>_Show the Nix configuration (nix show-config)</attribute>\
            \        <attribute name='action'>show_nix_config</attribute>\
            \      </item>\
            \      <item>\
            \        <attribute name='label'>_Upgrade Nix to the latest stable version (nix upgrade-nix)</attribute>\
            \        <attribute name='action'>upgrade_nix</attribute>\
            \      </item>\
            \      <item>\
            \        <attribute name='label'>_See the status of available packages (nix-env -qas)</attribute>\
            \        <attribute name='action'>see_the_status_of_available_packages</attribute>\
            \      </item>\
            \    </section>\
            \  </menu>\
            \</interface>"

getNixCmdArguments :: String -> (String, [String])
getNixCmdArguments  "list_installed_packages" = ("nix-env", ["-q"])
getNixCmdArguments  "nixos_version"           = ("nixos-version", [""])
getNixCmdArguments  "nix_version"           = ("nix", ["--version"])
--getNixCmdArguments  "view_the_set_of_available_packages_in_nixpkgs"           = ("nix-env", ["-qa"])
getNixCmdArguments  "see_the_status_of_available_packages"           = ("nix-env", ["-qas"])
getNixCmdArguments  "upgrade_all-packages_for_which_there_are_newer_versions_dry_run"           = ("nix-env", ["-u","--dry-run"])
getNixCmdArguments  "upgrade_all-packages_for_which_there_are_newer_versions"           = ("nix-env", ["-u"])
getNixCmdArguments  "list_all_generations"           = ("nix-env", ["--list-generations"])
getNixCmdArguments  "undo_nix_env_operation"           = ("nix-env", ["--rollback"])
getNixCmdArguments  "delete_all_old_gens_curr_profile"           = ("nix-env", ["--delete-generations" ,"old"])
getNixCmdArguments  "run_gc"           = ("nix-store", ["--gc"] )
getNixCmdArguments  "run_gc_dry_run"           = ("nix-store", ["--gc","--print-dead"] )
getNixCmdArguments  "delete_old_gens_of_all_profiles"           = ("nix-collect-garbage", ["-d"] )
getNixCmdArguments  "obtain_latest_nix_expressions"           = ("nix-channel", ["--update"] )
getNixCmdArguments  "show_nix_config"           = ("nix", ["show-config"] )
getNixCmdArguments  "upgrade_nix"           = ("nix", ["upgrade-nix"] )


actionActivated :: Gio.ActionEntryActivateFieldCallback_WithClosures
actionActivated action _ userData = do
  withTransient Gtk.Window (castPtr userData) $ \parent -> do
    actionName <- get action #name >>= return . fromJust

    let nixCmdArgs = getNixCmdArguments (unpack actionName)
    root_list_box <- new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeNone ]
    fs <- execNixCommand (fst nixCmdArgs) (snd nixCmdArgs)
    forM_ fs $ \f -> makeListRow f >>= #add  root_list_box

    scrollable <- new Gtk.ScrolledWindow []
    #add scrollable root_list_box
    boxResults <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
    #packStart boxResults scrollable True True 40


    cc <- containerGetChildren parent
    box <- castTo Gtk.Box (cc !! 0)
    let box' = fromJust box
    boxChildern <- containerGetChildren box'
    #remove box' (last boxChildern)
    #packStart box' boxResults True True 40

    #showAll parent


newActionEntry :: ActionEntryInfo -> IO Gio.ActionEntry
newActionEntry (name, callback) = do
  callback <- ( Gio.mk_ActionEntryActivateFieldCallback
              . Gio.wrap_ActionEntryActivateFieldCallback Nothing
              ) callback
  name <- textToCString name
  new Gio.ActionEntry [#name := name, #activate := callback]

addActionEntries :: forall a. GObject a => Gio.ActionMap -> [ActionEntryInfo] -> a -> IO ()
addActionEntries actionMap entryInfos ptr = do
  ptr <- unsafeManagedPtrCastPtr ptr
  (flip $ Gio.actionMapAddActionEntries actionMap) ptr =<< mapM newActionEntry entryInfos

castWOMaybe :: forall o o'. (GObject o, GObject o') => (ManagedPtr o' -> o') -> o -> IO o'
castWOMaybe typeToCast obj = castTo typeToCast obj >>= return . fromJust

getCastedObjectFromBuilder :: forall a. GObject a => Gtk.Builder -> Text -> (ManagedPtr a -> a) -> IO a
getCastedObjectFromBuilder builder name typeToCast = #getObject builder name
                                                   >>= return . fromJust
                                                   >>= castWOMaybe typeToCast

maybeGVariantFromText :: Text -> IO (Maybe GVariant)
maybeGVariantFromText text = Just <$> gvariantFromText text

addSectionWithNameSpace :: Gio.MenuModel -> Text -> Gio.Menu -> IO ()
addSectionWithNameSpace model namespace menu = do
  section <- Gio.menuItemNewSection Nothing model
  #setAttributeValue section "action-namespace" =<< maybeGVariantFromText namespace
  #appendItem menu section

appActivate:: Gtk.Application -> IO ()
appActivate app = do
  windows <- #getWindows app
  when (null windows) $ do
    win <- new Gtk.ApplicationWindow [ #application := app
                                     , #defaultWidth := 700
                                     , #defaultHeight := 600
                                     ]
    docActions <- new Gio.SimpleActionGroup []
    actMap <- Gio.toActionMap docActions
    addActionEntries actMap docEntryInfos win

    builder <- Gtk.builderNewFromString menuUIStr (-1)
    docMenu <- getCastedObjectFromBuilder builder "doc-menu" Gio.MenuModel
    buttonMenu <- new Gio.Menu []

    addSectionWithNameSpace docMenu "doc" buttonMenu

    button <- new Gtk.MenuButton [ #label := "Nix ..."
                                 , #halign := Gtk.AlignEnd
                                 , #valign := Gtk.AlignFill
                                 ]
    #insertActionGroup button "doc" =<< castTo Gio.ActionGroup docActions

    box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 20 ]
    #add win box


    #setMenuModel button =<< castTo Gio.MenuModel buttonMenu
    #add box button

    searchBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal ]
    searchInput <- new Gtk.Entry  [ #widthChars := 100 ]
    #packStart searchBox searchInput False False 10
    btn <- new Gtk.Button [ #label := "Search Nix packages (nix-env -qaP)" ]
    #packStart searchBox btn False False 10
    on btn #clicked (searchNixPackages searchInput box win)  -- TODO get from searchInput
    #add box searchBox

    scrollable <- new Gtk.ScrolledWindow []

    root_list_box <- new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeNone ]

    #add scrollable root_list_box
    boxResults <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
    #packStart boxResults scrollable True True 40
    #packStart box boxResults True True 40


    #showAll win


getInput :: Gtk.Entry -> IO Text
getInput = ( `get` #text)

execNixCommand :: String -> [String] -> IO [String]
execNixCommand nixCmd cmdArgs = do
  cmdOutput <- readProcess nixCmd cmdArgs   ""
  let cmdLines = lines cmdOutput
  return  cmdLines

searchNixPackages :: Gtk.Entry -> Gtk.Box -> Gtk.ApplicationWindow -> IO ()
searchNixPackages searchForEntry box win = do
    scrollable <- new Gtk.ScrolledWindow []

    searchFor <- getInput searchForEntry
    root_list_box <- new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeNone ]
    fs <- execNixCommand "nix-env" ["-qaP",unpack searchFor]
    forM_ fs $ \f -> makeListRow f >>= #add root_list_box

    #add scrollable root_list_box

    boxChildern <- containerGetChildren box
    #remove box (last boxChildern)

    boxResults <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
    #packStart boxResults scrollable True True 40
    #packStart box boxResults True True 40

    #showAll win


makeListRow :: String -> IO Gtk.ListBoxRow
makeListRow outputLine = do
    row <- new Gtk.ListBoxRow []
    list_box0 <- new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeNone ]
    #add row list_box0

    let
      labelText = pack outputLine

      add_file_label :: IO ()
      add_file_label = do
        lbl <- new Gtk.Label [ #label := labelText, #halign := Gtk.AlignStart, #marginLeft := 20 ]
        void (#add list_box0 lbl)

    add_file_label

    return row

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "nixos.nix-mate"]
  on app #activate $ appActivate app

  #run app Nothing
  return ()

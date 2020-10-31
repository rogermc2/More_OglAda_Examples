
with Settings;

package Settings.Loader is

   Settings_Loader_Exception : Exception;

   procedure Load_Settings (theSettings : in out Settings.Settings_Data);

end Settings.Loader;

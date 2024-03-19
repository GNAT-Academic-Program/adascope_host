with Gtk.Oscilloscope;  use Gtk.Oscilloscope;

package Worker is

   task type Process is
      entry Start (
         Scope     : Gtk_Oscilloscope;
         Channel_1 : Channel_Number;
         Channel_2 : Channel_Number;
         Channel_3 : Channel_Number
      );
      entry Stop;
   end Process;
end Worker;

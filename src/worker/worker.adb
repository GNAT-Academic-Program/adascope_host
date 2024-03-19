with Ada.Calendar;      use Ada.Calendar;
with Ada.Exceptions;    use Ada.Exceptions;
with Gtk.Main.Router;   use Gtk.Main.Router;
with Ada.Text_IO;       use Ada.Text_IO;
with Glib;              use Glib;

with GNAT.Serial_Communications;
with Globals;

use type Globals.Board_State;

package body Worker is

   --  X and Y value pairs to be displayed in the graph
   X : Integer := 0;
   Y : Float   := 0.0;

   --  Feeding data to oscilloscope
   procedure Feed_UART_Data (
      Scope     : Gtk_Oscilloscope;
      Channel   : Channel_Number
   ) is
   begin

      --  Feed data to the graph
      for Index in 1 .. Globals.Number_Of_Samples / 2 loop
         X := Index;
         Y := Globals.Processed_Data.Get_Data_Point (
            Channel => Integer'Val (Channel),
            Index   => Index
         );

         Scope.Feed (
            Channel => Channel,
            T       => Gdouble (X),
            V       => Gdouble (Y)
         );
      end loop;
   end Feed_UART_Data;

   --  Managing pause/play
   task body Process is
      Scope     : Gtk_Oscilloscope;
      Channel_1 : Channel_Number;
      Channel_2 : Channel_Number;
      Channel_3 : Channel_Number;
      Last_Time : Time := Clock;

   begin

      --  Waiting for parameters or exit request
      select
         accept Start (
            Scope     : Gtk_Oscilloscope;
            Channel_1 : Channel_Number;
            Channel_2 : Channel_Number;
            Channel_3 : Channel_Number
         )
         do
            Process.Scope     := Scope;
            Process.Channel_1 := Channel_1;
            Process.Channel_2 := Channel_2;
            Process.Channel_3 := Channel_3;
            Put_Line ("Start process");
         end Start;

      or accept Stop;
         raise Quit_Error;
      end select;

      Put_Line ("Starting computations");

      --  Starting computations
      while True loop

         --  Updating each 200ms
         if Clock - Last_Time > 0.2 then
            select
               accept Stop; -- Check if existing is requested
               raise Quit_Error;
            else
               Last_Time := Clock;

               --  check if state is connected
               if Globals.Board_State_Change.Get_Board_State =
                  Globals.Connected
               then
                  Feed_UART_Data (Scope, Channel_1);
                  Feed_UART_Data (Scope, Channel_2);
                  Feed_UART_Data (Scope, Channel_3);
               end if;
            end select;
         end if;

      end loop;
      Put_Line ("Invalid loop end");
      accept Stop;

   exception
      when Quit_Error | Busy_Error => --  Main loop quitted, we follow
         Put_Line ("Quitting process");
         null;

      when GNAT.Serial_Communications.Serial_Error =>
         Put_Line ("Serial Error");
         null;

      when Error : others =>
         Put_Line ("Error in process");
         Say (Exception_Information (Error));
      Put_Line ("Ending process");
   end Process;
end Worker;

with Gtk.Oscilloscope;      use Gtk.Oscilloscope;
with Gtk.Main.Router;       use Gtk.Main.Router;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Calendar;          use Ada.Calendar;
with Ada.Text_IO;           use Ada.Text_IO;
with Gtk.Missed;            use Gtk.Missed;
with Gtk.Widget;            use Gtk.Widget;
with Gtk.Window;            use Gtk.Window;
with Gdk.Color;             use Gdk.Color;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Frame;             use Gtk.Frame;
with Gtk.Table;             use Gtk.Table;
with Gtk.Box;               use Gtk.Box;
with Glib;                  use Glib;

with Gtk.Oscilloscope.Channels_Panel;
   use Gtk.Oscilloscope.Channels_Panel;
with Gtk.Oscilloscope.Sweeper_Panel;
   use Gtk.Oscilloscope.Sweeper_Panel;
with GNAT.Serial_Communications;
with Ada.Unchecked_Conversion;
with Gtk.Layered;
with Globals;
with Worker;
with Uart;

use type Globals.Board_State;

procedure User_Interface is
   Window            : Gtk_Window;
   Oscilloscope      : Gtk_Oscilloscope;
   UART_obj          : Uart.Read;
   Writer_Ch_1       : Worker.Process;
   Writer_Ch_2       : Worker.Process;
   Writer_Ch_3       : Worker.Process;
   Channel_1         : Channel_Number;
   Channel_2         : Channel_Number;
   Channel_3         : Channel_Number;
   Last_Time         : Time := Clock;
   Is_connected      : Boolean;
   Has_Been_Notified : Boolean;
   Exiting_Window    : Boolean := False;
   Port_Location     : constant
                          GNAT.Serial_Communications.Port_Name :=
                          "/dev/ttyACM0";

   task Connect_Oscilloscope;

--
--  Connect_Oscilloscope
--
   task body Connect_Oscilloscope is
   begin
      Has_Been_Notified := False;
      while Exiting_Window = False loop
         --  Checking connection every second
         if Clock - Last_Time > 1.0 then
            if Globals.Board_State_Change.Get_Board_State =
               Globals.Disconnected
            then
               Is_connected := True;
               declare
               begin
                  GNAT.Serial_Communications.Open
                  (Port => Globals.Port,
                     Name => Port_Location);

                  GNAT.Serial_Communications.Set
                     (Port => Globals.Port,
                     Rate => GNAT.Serial_Communications.B921600);
               exception
                  when GNAT.Serial_Communications.Serial_Error =>
                     Put_Line ("Serial Error - Board not connected");
                     Is_connected := False;
                     if Has_Been_Notified = False then
                        Say ("No board was detected. " &
                           "Make sure you connect a board to " &
                           "the host computer before hitting " &
                           "the start button."
                        );
                        Has_Been_Notified := True;
                     end if;
               end;
               if Is_connected then
                  Put_Line ("Changing board state to Connected");
                  Globals.Board_State_Change.Change_State_Connected;
                  UART_obj.Start;
               end if;
            else
               Put_Line ("Board connected.");
               exit when
                   Globals.Board_State_Change.Get_Board_State =
                      Globals.Connected;
            end if;
            Last_Time := Clock;
         end if;
      end loop;
   end Connect_Oscilloscope;

--
--  Start_Oscilloscope
--
   procedure Start_Oscilloscope is
      use Gtk.Main.Router;
      From  : Gdouble;
      To    : Gdouble;
      Width : Gdouble;
   begin
      From  := Gdouble (0);   -- X axis start value
      To    := Gdouble (600); -- X axis end value
      Width := To - From;

      --  Set page size of the scope
      Oscilloscope.Get_Sweeper (Lower).Configure
      (Value   => From,
         Lower => From,
         Upper => To,
         Step_Increment => Width / 100.0,
         Page_Increment => Width / 10.0,
         Page_Size      => Width / 2.0
      );
      --  Protected object
      --  Initiate writing process channel 1
      Writer_Ch_1.Start
      (
         Oscilloscope,
         Channel_1,
         Channel_2,
         Channel_3
      );
   exception
      when Error : Data_Error =>
         Say (Exception_Message (Error));
      when Error : others =>
         Say (Exception_Information (Error));
   end Start_Oscilloscope;

--
--  Delete_Event -- Window closing notification event
--
   function Delete_Event return Boolean is
   begin
      Writer_Ch_1.Stop; -- Stop the computation process
      Writer_Ch_2.Stop; -- Stop the computation process
      Writer_Ch_3.Stop; -- Stop the computation process
      return False;    -- Confirm completion exception
   exception
      when Tasking_Error =>
         return False;
   end Delete_Event;

   type Local_Delete_Callback is access function  return Boolean;
   function "+" is
      new Ada.Unchecked_Conversion
         (Local_Delete_Callback,
            Cb_Gtk_Widget_Gdk_Event_Boolean
         );
begin
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Gtk.Main.Router.Init (Window);
   Window.Set_Title ("AdaScope");
   Window.On_Delete_Event (+Delete_Event'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);

   declare
      Pane           : Gtk_Hbox;                        --  Main panel
      Panels         : Gtk_Table;                       --  Channels panel
      Frame          : Gtk_Frame;                       --  Oscilloscope
      Channels       : Gtk_Oscilloscope_Channels_Panel; --  Channel checkboxes
      Sweeper        : Gtk_Oscilloscope_Sweeper_Panel;
      --  X_Axis_Range   : Gtk_Entry;
   begin

      --  Connect_Oscilloscope;

      --
      --  Setting up the main window pane
      --
      Gtk_New_Hbox (Pane);
      Pane.Set_Spacing (3);
      Window.Add (Pane);
      --
      --  Initialize oscilloscope widget
      --
      Gtk_New (Widget => Oscilloscope);
      Oscilloscope.Set_Manual_Sweep (False);
      --
      --  Configuring the lower axis
      --    No sweeping
      --    Yes scale (slider)
      --    Grid
      --    Visible as plain numbers
      --
      Oscilloscope.Set_Frozen       (Lower, True);
      Oscilloscope.Set_Time_Scale   (Lower, True);
      Oscilloscope.Set_Time_Grid    (Lower, True);
      Oscilloscope.Set_Time_Axis    (Lower, True, False);
      Oscilloscope.Set_Grid_Colors  (
         RGB (0.75, 0.75, 0.75),
         RGB (0.9, 0.9, 0.9));

      declare
      begin
         Channel_1 :=
            Add_Channel
            (Widget     => Oscilloscope,
               Mode     => Gtk.Layered.Linear,
               Sweeper  => Lower,
               Color    => RGB (0.0, 0.0, 1.0)
            );
         Channel_2 :=
            Add_Channel
            (Widget     => Oscilloscope,
               Mode     => Gtk.Layered.Linear,
               Sweeper  => Lower,
               Color    => RGB (1.0, 0.0, 0.0),
               Group    => Oscilloscope.Get_Group (Channel_1)
            );
         Channel_3 :=
            Add_Channel
            (Widget     => Oscilloscope,
               Mode     => Gtk.Layered.Linear,
               Sweeper  => Lower,
               Color    => RGB (1.0, 0.0, 1.0),
               Group    => Oscilloscope.Get_Group (Channel_1)
            );
      end;
      --
      --  Configuring the left axis for this channel (and its group)
      --
      Oscilloscope.Set_Group (Left, Oscilloscope.Get_Group (Channel_1));
      Oscilloscope.Set_Values_Axis  (Left, True);
      Oscilloscope.Set_Values_Scale (Left, True);
      Oscilloscope.Set_Values_Grid  (Left, True);
      Oscilloscope.Set_Values_Axis_Width (Left, 80);

      Gtk_New (Frame);
      Pane.Pack_Start (Frame);
      Frame.Set_Border_Width (3);
      Frame.Add (Oscilloscope);
      Frame.Set_Size_Request (300, 210);

      Gtk_New (Panels, 4, 1, False);
      Pane.Pack_Start (Panels, False, False);

      --  Channels
      Gtk_New (Frame, "Channels");
      Panels.Add (Frame);
      Gtk_New (Channels, Oscilloscope);
      Channels.Set_Border_Width (3);
      Frame.Add (Channels);

      --  Input
      Gtk_New (Frame, "Lower sweeper");
      Panels.Attach (Frame, 0, 1, 2, 3, Yoptions => Shrink or Fill);
      Gtk_New (Sweeper, Oscilloscope, Lower);
      Sweeper.Set_Border_Width (3);
      Frame.Add (Sweeper);

   end;
   declare
   begin
      Start_Oscilloscope;
   end;
   Window.Set_Default_Size (1200, 600);
   Show_All (Window);
   Gtk.Main.Main;
   Put_Line ("Window closed");
   Exiting_Window := True;
   GNAT.Serial_Communications.Close (Globals.Port);
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end User_Interface;

with Ada.Text_IO;
with Interfaces; use Interfaces;

with HAL;

with Lv; use Lv;
with Lv.Color;
with Lv.Hal.Disp;     use Lv.Hal.Disp;
with Lv.Hal.Indev;     use Lv.Hal.Indev;
with Lv.Indev;
with Lv.Objx;
with Lv.Objx.Img;
with Lv.Vdb;

with BB_Pico_Bsp.LCD;
with BB_Pico_Bsp.Keyboard;
with BB_Pico_Bsp.Touch;
with BBQ10KBD;

package body BB_Pico_Bsp.LVGL_Backend is

   LV_Disp_Drv : aliased Disp_Drv_T;
   LV_Disp : Disp_T;

   LV_Indev_Pointer_Drv : aliased Indev_Drv_T;
   LV_Indev_Keypad_Drv : aliased Indev_Drv_T;
   LV_Indev_Keypad  : Indev_T;
   LV_Indev_Pointer : Indev_T;
   Cursor_Obj : Lv.Objx.Img.Instance;

   Pointer : Indev_Data_T;

   function Read_Pointer (Data : access Indev_Data_T) return U_Bool
     with Convention => C;

   function Read_Keypad (Data : access Indev_Data_T) return U_Bool
     with Convention => C;

   procedure Disp_Flush
     (X1    : Int32_T;
      Y1    : Int32_T;
      X2    : Int32_T;
      Y2    : Int32_T;
      Color : access constant Color_Array)
   with Convention => C;

   procedure Disp_Fill
     (X1    : Int32_T;
      Y1    : Int32_T;
      X2    : Int32_T;
      Y2    : Int32_T;
      Color : Lv.Color.Color_T)
   with Convention => C;

   procedure Disp_Map
     (X1    : Int32_T;
      Y1    : Int32_T;
      X2    : Int32_T;
      Y2    : Int32_T;
      Color : access constant Color_Array)
   with Convention => C;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Enable_Pointer : Boolean := True) is
      Mouse_Cursor_Icon : Integer;
      pragma Import (C, Mouse_Cursor_Icon, "mouse_cursor_icon");
   begin

      Lv.Hal.Disp.Init_Drv (LV_Disp_Drv'Access);

      LV_Disp_Drv.Disp_Flush := Disp_Flush'Access;
      LV_Disp_Drv.Disp_Fill := Disp_Fill'Access;
      LV_Disp_Drv.Disp_Map := Disp_Map'Access;

      LV_Disp := Lv.Hal.Disp.Register (LV_Disp_Drv'Access);
      Lv.Hal.Disp.Set_Active (LV_Disp);

      Pointer.Union.Point := (0, 0);
      Pointer.State := Lv.Hal.Indev.State_Rel;

      Lv.Hal.Indev.Init_Drv (LV_Indev_Keypad_Drv'Access);
      LV_Indev_Keypad_Drv.Read := Read_Keypad'Access;
      LV_Indev_Keypad_Drv.C_Type := Lv.Hal.Indev.Type_Keypad;
      LV_Indev_Keypad := Lv.Hal.Indev.Register (LV_Indev_Keypad_Drv'Access);

      Lv.Hal.Indev.Init_Drv (LV_Indev_Pointer_Drv'Access);
      LV_Indev_Pointer_Drv.Read := Read_Pointer'Access;
      LV_Indev_Pointer_Drv.C_Type := Lv.Hal.Indev.Type_Pointer;
      LV_Indev_Pointer := Lv.Hal.Indev.Register (LV_Indev_Pointer_Drv'Access);

      if Enable_Pointer then
         Cursor_Obj := Lv.Objx.Img.Create (Lv.Objx.Scr_Act, Lv.Objx.No_Obj);
         Lv.Objx.Img.Set_Src (Cursor_Obj, Mouse_Cursor_Icon'Address);
         Lv.Indev.Set_Cursor (LV_Indev_Pointer, Cursor_Obj);
      end if;
   end Initialize;

   ------------------
   -- Keypad_Indev --
   ------------------

   function Keypad_Indev return Lv.Hal.Indev.Indev_T
   is (LV_Indev_Keypad);

   ------------------
   -- Read_Pointer --
   ------------------

   function Read_Pointer (Data : access Indev_Data_T) return U_Bool is
   begin

      Pointer.State := Lv.Hal.Indev.State_Rel;

      for Elt of BB_Pico_Bsp.Touch.Get_All_Touch_Points loop
         if Elt.Weight /= 0 then
            Pointer.Union.Point.X := Integer_16 (Elt.X);
            Pointer.Union.Point.Y := Integer_16 (Elt.Y);
            Pointer.State := Lv.Hal.Indev.State_Pr;
         end if;
      end loop;

      Data.all := Pointer;

      return 0;
   end Read_Pointer;

   -----------------
   -- Read_Keypad --
   -----------------

   function Read_Keypad (Data : access Indev_Data_T) return U_Bool is
      use BBQ10KBD;

      State : Key_State;
   begin
      loop
         State := BB_Pico_Bsp.Keyboard.Key_FIFO_Pop;

         case State.Kind is
            when Error =>
               return 0; -- No more events
            when Held_Pressed =>
               null; -- LVGL doesn't have a held pressed event
            when others =>
               declare
                  Pressed : constant Boolean := State.Kind = BBQ10KBD.Pressed;
               begin
                  case State.Code is
                  when Keyboard.KEY_JOY_UP =>
                     Data.Union.Key := Lv.LV_KEY_UP;
                  when Keyboard.KEY_JOY_DOWN =>
                     Data.Union.Key := Lv.LV_KEY_DOWN;
                  when Keyboard.KEY_JOY_LEFT =>
                     Data.Union.Key := Lv.LV_KEY_LEFT;
                  when Keyboard.KEY_JOY_RIGHT =>
                     Data.Union.Key := Lv.LV_KEY_RIGHT;
                  when Keyboard.KEY_JOY_CENTER =>
                     Data.Union.Key := Lv.LV_KEY_ENTER;
                  when Keyboard.KEY_BTN_LEFT1 =>
                     Data.Union.Key := Lv.LV_KEY_PREV;
                  when Keyboard.KEY_BTN_LEFT2 =>
                     Data.Union.Key := Lv.LV_KEY_HOME;
                  when Keyboard.KEY_BTN_RIGHT1 =>
                     Data.Union.Key := Lv.LV_KEY_BACKSPACE;
                  when Keyboard.KEY_BTN_RIGHT2 =>
                     Data.Union.Key := Lv.LV_KEY_NEXT;
                  when others =>
                     Data.Union.Key := Unsigned_32 (State.Code);
                  end case;

                  Data.State := (if Pressed
                                 then Lv.Hal.Indev.State_Pr
                                 else Lv.Hal.Indev.State_Rel);
                  return 1;
               end;
         end case;
      end loop;
   end Read_Keypad;

   ----------------
   -- Disp_Flush --
   ----------------

   procedure Disp_Flush
     (X1    : Int32_T;
      Y1    : Int32_T;
      X2    : Int32_T;
      Y2    : Int32_T;
      Color : access constant Color_Array)
   is

      Width  : constant Natural := LCD.Width;
      Height : constant Natural := LCD.Height;
      Len : constant Int32_T := (X2 - X1 + 1) * (Y2 - Y1 + 1);
   begin
      LCD.Wait_For_DMA;

      if X2 < 0
        or else
         Y2 < 0
        or else
         X1 > Int32_T (Width - 1)
        or else
         Y1 > Int32_T (Height - 1)
      then
         Lv.Vdb.Flush_Ready;
         return;
      end if;

      LCD.Send_Pixels (Natural (X1),
                       Natural (Y1),
                       Natural (X2),
                       Natural (Y2),
                       Color.all'Address,
                       HAL.UInt32 (Len),
                       Blocking => False);

      Lv.Vdb.Flush_Ready;
   end Disp_Flush;

   ---------------
   -- Disp_Fill --
   ---------------

   procedure Disp_Fill
     (X1    : Int32_T;
      Y1    : Int32_T;
      X2    : Int32_T;
      Y2    : Int32_T;
      Color : Lv.Color.Color_T)
   is

      C      : constant Uint16_T := Lv.Color.Color_To16 (Color);
      pragma Unreferenced (C);
      Width  : constant Natural := LCD.Width;
      Height : constant Natural := LCD.Height;
      Len    : constant Int32_T := (X2 - X1) * (Y2 - Y1);
      pragma Unreferenced (Len);
   begin
      if X2 < 0
        or else
         Y2 < 0
        or else
         X1 > Int32_T (Width - 1)
        or else
         Y1 > Int32_T (Height - 1)
      then
         Lv.Vdb.Flush_Ready;
         return;
      end if;

      Lv.Vdb.Flush_Ready;
   end Disp_Fill;

   --------------
   -- Disp_Map --
   --------------

   procedure Disp_Map
     (X1    : Int32_T;
      Y1    : Int32_T;
      X2    : Int32_T;
      Y2    : Int32_T;
      Color : access constant Color_Array)
   is
   begin
      Ada.Text_IO.Put_Line ("X1:" & X1'Img);
      Ada.Text_IO.Put_Line ("Y1:" & Y1'Img);
      Ada.Text_IO.Put_Line ("X2:" & X2'Img);
      Ada.Text_IO.Put_Line ("Y2:" & Y2'Img);
      Ada.Text_IO.Put_Line ("Length:" & Color'Length'Img);
      Ada.Text_IO.Put_Line ("First:" & Color'First'Img);
   end Disp_Map;

end BB_Pico_Bsp.LVGL_Backend;

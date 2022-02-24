with RP.Device;
with BB_Pico_Bsp.LVGL_Backend;
with BB_Pico_Bsp.LCD;

with Lv.Tasks;
with Lv.Hal.Tick;

with lvgl_ada_Config; use lvgl_ada_Config;

with Lv; use Lv;
with Lv.Area;
with Lv.Group;
with Lv.Objx; use Lv.Objx;
with Lv.Objx.Cont;
with Lv.Objx.Tabview;
with Lv.Objx.Page;
with Lv.Objx.Slider;
with Lv.Objx.Textarea;
with Lv.Objx.Roller;
with Lv.Objx.Chart;
with Lv.Objx.Gauge;
with Lv.Objx.Preload;
with Lv.Objx.Calendar;

with Lv.Color;
with Lv.Theme;
with Lv.Font;
with Lv.Indev;

with Lv.Strings; use Lv.Strings;

package body LVGL_Demo is

   use type Int16_T;

   Theme_Roller : Roller.Instance;

   GP : Lv.Group.Instance;

   procedure Create_Theme_Tab (Parent : Page.Instance);
   procedure Create_Editor_Tab (Parent : Page.Instance);
   procedure Create_Status_Tab (Parent : Page.Instance);
   procedure Create_Cal_Tab (Parent : Page.Instance);

   procedure Init_Themes (Hue : Lv.Theme.Hue_T);

   function Roller_Action (Arg1 : Obj_T) return Res_T
     with Convention => C;

   function Slider_Action (Arg1 : Obj_T) return Res_T
     with Convention => C;

   -----------------
   -- Init_Themes --
   -----------------

   procedure Init_Themes (Hue : Lv.Theme.Hue_T) is
      Unused : Lv.Theme.Theme;
   begin
      Unused := Lv.Theme.Default_Init (Hue, Lv.Font.No_Font);
      Unused := Lv.Theme.Material_Init (Hue, Lv.Font.No_Font);
      Unused := Lv.Theme.Mono_Init (Hue, Lv.Font.No_Font);
      Unused := Lv.Theme.Alien_Init (Hue, Lv.Font.No_Font);
      Unused := Lv.Theme.Nemo_Init (Hue, Lv.Font.No_Font);
      Unused := Lv.Theme.Night_Init (Hue, Lv.Font.No_Font);
      Unused := Lv.Theme.Zen_Init (Hue, Lv.Font.No_Font);
   end Init_Themes;

   -------------------
   -- Roller_Action --
   -------------------

   function Roller_Action (Arg1 : Obj_T) return Res_T is
   begin
      case Roller.Selected (Arg1) is
         when 0       => Lv.Theme.Set_Current (Lv.Theme.Get_Default);
         when 1       => Lv.Theme.Set_Current (Lv.Theme.Get_Material);
         when 2       => Lv.Theme.Set_Current (Lv.Theme.Get_Mono);
         when 3       => Lv.Theme.Set_Current (Lv.Theme.Get_Alien);
         when 4       => Lv.Theme.Set_Current (Lv.Theme.Get_Night);
         when 5       => Lv.Theme.Set_Current (Lv.Theme.Get_Zen);
         when others  => Lv.Theme.Set_Current (Lv.Theme.Get_Nemo);
      end case;

      return Res_Ok;
   end Roller_Action;

   -------------------
   -- Slider_Action --
   -------------------

   function Slider_Action (Arg1 : Obj_T) return Res_T is
   begin
      Init_Themes (Uint16_T (Slider.Value (Arg1)));
      return Roller_Action (Theme_Roller);
   end Slider_Action;

   ----------------------
   -- Create_Theme_Tab --
   ----------------------

   procedure Create_Theme_Tab (Parent : Page.Instance) is
      TH  : constant Theme.Theme := Theme.Get_Current;
      pragma Unreferenced (TH);
      Slide : Slider.Instance;
   begin
      Page.Set_Scrl_Layout (Parent, Cont.Layout_Pretty);

      Theme_Roller := Roller.Create (Parent, No_Obj);
      Roller.Set_Options (Theme_Roller, New_String ("Default" & ASCII.LF &
                                           "Material" & ASCII.LF &
                                           "Mono" & ASCII.LF &
                                           "Alien" & ASCII.LF &
                                           "Night" & ASCII.LF &
                                           "Zen" & ASCII.LF &
                                           "Nemo"));
      Roller.Set_Selected (Theme_Roller, 4, 0);
      Roller.Set_Visible_Row_Count (Theme_Roller, 3);
      Roller.Set_Action (Theme_Roller, Roller_Action'Access);

      Slide := Slider.Create (Parent, No_Obj);
      Slider.Set_Action (Slide, Slider_Action'Access);
      Slider.Set_Range (Slide, 0, 360);
      Slider.Set_Value (Slide, 70);

   end Create_Theme_Tab;

   -----------------------
   -- Create_Editor_Tab --
   -----------------------

   procedure Create_Editor_Tab (Parent : Page.Instance) is
      W  : constant Lv.Area.Coord_T := Page.Scrl_Width (Parent);
      TA : Textarea.Instance;
   begin

      TA := Textarea.Create (Parent, No_Obj);
      Set_Size (TA, W, BB_Pico_Bsp.LCD.Height - 50);
      Align (TA, No_Obj, Align_In_Top_Right, 0, 0);
      Textarea.Set_Cursor_Type (TA, Textarea.Cursor_Block);
      Textarea.Set_Text
        (TA,
         New_String ("with Ada.Text_IO;" & ASCII.LF &
           "procedure Hello is" & ASCII.LF &
           "begin" & ASCII.LF &
           "   Ada.Text_IO.Put_Line (""Hello world!"");" & ASCII.LF &
           "end Hello;" & ASCII.LF));
      Lv.Group.Add_Obj (GP, TA);
      Lv.Group.Focus_Obj (TA);
   end Create_Editor_Tab;

   -----------------------
   -- Create_Status_Tab --
   -----------------------

   procedure Create_Status_Tab (Parent : Page.Instance) is
      W  : constant Lv.Area.Coord_T := Page.Scrl_Width (Parent);
      Ch : Chart.Instance;
      S1 : Chart.Series;
      G  : Gauge.Instance;
      LD : Preload.Instance;
   begin
      Ch := Chart.Create (Parent, No_Obj);
      Set_Size (Ch, W / 3, Vertical_Resolution / 3);
      Set_Pos (Ch, Density_Per_Inch / 10, Density_Per_Inch / 10);

      S1 := Chart.Add_Series (Ch, Lv.Color.Color_Red);
      Chart.Set_Next (Ch, S1, 30);
      Chart.Set_Next (Ch, S1, 20);
      Chart.Set_Next (Ch, S1, 10);
      Chart.Set_Next (Ch, S1, 12);
      Chart.Set_Next (Ch, S1, 20);
      Chart.Set_Next (Ch, S1, 27);
      Chart.Set_Next (Ch, S1, 35);
      Chart.Set_Next (Ch, S1, 55);
      Chart.Set_Next (Ch, S1, 70);
      Chart.Set_Next (Ch, S1, 75);

      LD := Preload.Create (Parent, No_Obj);
      Align (LD, Ch, Align_Out_Right_Mid, 15, 0);

      G := Gauge.Create (Parent, No_Obj);
      Gauge.Set_Value (G, 0, 40);
      Set_Size (G, W / 4, W / 4);
      Align (G, LD, Align_Out_Right_Mid, 15, 0);
   end Create_Status_Tab;

   --------------------
   -- Create_Cal_Tab --
   --------------------

   procedure Create_Cal_Tab (Parent : Page.Instance) is
      W  : constant Lv.Area.Coord_T := Page.Scrl_Width (Parent);
      Cal    : Calendar.Instance;
      Highlighted_days : aliased Calendar.Date_Array :=
        ((2022, 2, 23), (2022, 2, 15));
   begin
      --  Create a Calendar
      Cal := Calendar.Create (Parent, No_Obj);
      Set_Size (Cal, W, BB_Pico_Bsp.LCD.Height - 50);
      Set_Top (Cal, 1);

      Calendar.Set_Highlighted_Dates (Cal, Highlighted_days'Access, 2);
      Calendar.Set_Today_Date (Cal, Highlighted_days (0)'Access);
      Calendar.Set_Showed_Date (Cal, Highlighted_days (0)'Access);
   end Create_Cal_Tab;

   ----------------
   -- Initialize --
   ----------------

   procedure Run is
      Scr       : Cont.Instance;
      TV        : Tabview.Instance;
      Theme_Tab : Page.Instance;
      Tab1      : Page.Instance;
      Tab2      : Page.Instance;
      Tab3      : Page.Instance;
   begin
      Lv.Init;

      BB_Pico_Bsp.LVGL_Backend.Initialize;

      Init_Themes (220);
      Lv.Theme.Set_Current (Lv.Theme.Get_Night);

      Scr := Cont.Create (No_Obj, No_Obj);
      Scr_Load (Scr);

      GP := Lv.Group.Create;
      Lv.Indev.Set_Group (BB_Pico_Bsp.LVGL_Backend.Keypad_Indev, GP);

      TV := Tabview.Create (Scr, No_Obj);
      Set_Size (TV, Horizontal_Resolution, Vertical_Resolution);

      Tabview.Set_Anim_Time (TV, 0);

      Tab1 := Tabview.Add_Tab (TV, New_String ("Editor"));
      Tab2 := Tabview.Add_Tab (TV, New_String ("Calendar"));
      Tab3 := Tabview.Add_Tab (TV, New_String ("Status"));
      Theme_Tab := Tabview.Add_Tab (TV, New_String ("Theme"));

      Tabview.Set_Tab_Act (TV, 0, 0);

      Create_Theme_Tab (Theme_Tab);
      Create_Editor_Tab (Tab1);
      Create_Cal_Tab (Tab2);
      Create_Status_Tab (Tab3);

      loop
         Lv.Tasks.Handler;
         RP.Device.Timer.Delay_Milliseconds (1);
         Lv.Hal.Tick.Inc (1);
      end loop;
   end Run;

end LVGL_Demo;

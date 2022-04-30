unit main;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

    { TForm1 }

    TForm1 = class(TForm)
        Timer1: TTimer;
        procedure FormCreate(Sender: TObject);
        procedure FormPaint(Sender: TObject);
        procedure DrawPic();
        procedure FormResize(Sender: TObject);
        procedure Timer1Timer(Sender: TObject);
        procedure InvalidateMeasures();
    private

    public

    end;

    TDayTime = (dtDay, dtNight);

    //TTimeColors = record
    //    Atmo, Celestial: TColor;
    //end;

    TCircularBody = record
        x, y, r: integer;
    end;

var
    Form1: TForm1;

implementation
uses drawing;

var
    sun_x, sun_y, sun_y0, px, py, alpha: integer;
    //tcTime: TTimeColors;
    clAtmo: TColor;
const
    deg = PI / 180;
    Rc = 20;
    sun_x0 = Rc;
    //tcDay: TTimeColors = (Atmo: clWhite; Celestial: clYellow);
    //tcNight: TTimeColors = (Atmo: clBlack; Celestial: TColor($ccccff));
    clSun = clYellow;
    clMoon = TColor($ccccff);

{$R *.lfm}

{ TForm1 }

procedure SetTime(dt: TDayTime);
begin
    if (dt = dtDay) then
        clAtmo := clWhite
    else
        clAtmo := clBlack;
end;

procedure TForm1.InvalidateMeasures();
var
    R, dy2: Double;
    dx: integer;
begin
    py := Height;
    px := Width div 2;
    R := Height - Rc;
    dx := sun_x0 - px;
    dy2 := R * R - dx * dx;
    if dy2 < 0 then
        dy2 := 0;
    sun_y0 := Trunc(Height - sqrt(dy2));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    InvalidateMeasures();
    SetTime(dtDay);
    alpha := 0;
    sun_x := sun_x0;
    sun_y := sun_y0;
end;

procedure TForm1.DrawPic();
var
    moon_x, moon_y: integer;
begin
    Canvas.Brush.Color := clAtmo;
    Canvas.Clear;
    Canvas.Brush.Color := clSun;
    DrawCircle(Canvas, sun_x, sun_y, Rc);
    //Canvas.Ellipse(sun_x - Rc, sun_y - Rc, sun_x + Rc, sun_y + Rc);
    moon_x := Width - sun_x;
    moon_y := 2 * Height - sun_y;
    Canvas.Brush.Color := clMoon;
    DrawCircle(Canvas, moon_x, moon_y, Rc);
    //Canvas.Line(sun_x, sun_y, px, py);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
    InvalidateMeasures();
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
    vx, vy: integer;
    nvx, nvy, ralpha: Double;
begin
    alpha := alpha + 1;
    //Label1.Caption := IntToStr(alpha);
    vx := sun_x - px;
    vy := sun_y - py;
    ralpha := deg * alpha;
    nvx := vx * cos(ralpha) - vy * sin(ralpha);
    nvy := vx * sin(ralpha) + vy * cos(ralpha);
    sun_x := Trunc(nvx) + px;
    sun_y := Trunc(nvy) + py;
    if (alpha mod 180) = 0 then
        if alpha mod 360 = 0 then
        begin
            SetTime(dtDay);
            alpha := 0;
        end
        else
            SetTime(dtNight);
    DrawPic();
    sun_x := sun_x0;
    sun_y := sun_y0;

end;

procedure TForm1.FormPaint(Sender: TObject);
begin
    DrawPic();
end;

end.


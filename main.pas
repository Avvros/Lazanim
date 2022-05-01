unit main;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

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

    TCelestial = record
        x, y, r: integer;
        color: TColor;
    end;

var
    Form1: TForm1;

implementation
uses drawing, calc;

var
    sun_x, sun_y, sun_y0, sun_x0, px, py: integer;
    alpha, speed: double;
    clAtmo: TColor;
const
    deg = PI / 180;
    Rc = 20;
    clSun = clYellow;
    clMoon = TColor($dbdbff);

{$R *.lfm}

{ TForm1 }

procedure DrawAtmosphere();
begin

end;

procedure DrawBackground();
begin

end;

procedure SetTime(time: double);
begin
    clAtmo := LerpColor(clBlack, clWhite, time);
end;

procedure TForm1.InvalidateMeasures();
var
    R: integer;
begin
    py := Height;
    px := Width div 2;
    R := Height - Rc;
    sun_x0 := px - R;
    sun_y0 := Height;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    speed := 0.5;
    InvalidateMeasures();
    SetTime(1);
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
    moon_x := Width - sun_x;
    moon_y := 2 * Height - sun_y;
    Canvas.Brush.Color := clMoon;
    DrawCircle(Canvas, moon_x, moon_y, Rc);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
    InvalidateMeasures();
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
    vx, vy: integer;
    nvx, nvy, ralpha, sina, cosa: Double;
begin
    alpha := alpha + 1 * speed;
    vx := sun_x - px;
    vy := sun_y - py;
    ralpha := deg * alpha;
    sina := sin(ralpha);
    cosa := cos(ralpha);
    nvx := vx * cosa - vy * sina;
    nvy := vx * sina + vy * cosa;
    sun_x := Trunc(nvx) + px;
    sun_y := Trunc(nvy) + py;
    if (alpha > 360) then alpha := alpha - 360;
    SetTime((sina + 1) / 2);
    DrawPic();
    sun_x := sun_x0;
    sun_y := sun_y0;

end;

procedure TForm1.FormPaint(Sender: TObject);
begin
    DrawPic();
end;

end.


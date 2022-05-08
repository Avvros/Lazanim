unit main;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

    { TMainForm }

    TMainForm = class(TForm)
        Timer1: TTimer;
        procedure FormCreate(Sender: TObject);
        procedure FormPaint(Sender: TObject);
        procedure DrawPic();
        procedure DrawAtmosphere();
        procedure DrawBackground();
        procedure GenerateStars();
        procedure ResizeMountains();
        procedure FormResize(Sender: TObject);
        procedure Timer1Timer(Sender: TObject);
        procedure InvalidateMeasures();
    private

    public

    end;

var
    MainForm: TMainForm;

implementation
uses drawing, calc;

const
    deg = PI / 180;
    Rc = 20;
    clSun = clYellow;
    clMoon = TColor($dbdbff);
    starsCount = 50;
    mtCount = 4;
    maxHeight = 300;
    mtRatios : array [0..mtCount + 1] of array [0..1] of double = (
        (0, 0.75),
        (0.15, 1),
        (0.35, 0.5),
        (0.55, 0.9),
        (0.75, 0.6),
        (1, 0.8)
    );

type
    TStarsArr = array [0..starsCount - 1] of TPoint;
    TMountainsArr = array [0..mtCount + 1] of TPoint;

var
    sun_x, sun_y, sun_y0, sun_x0, px, py: integer;
    alpha, speed: double;
    clAtmo: TColor;
    Stars: TStarsArr;
    Mountains: TMountainsArr;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.DrawAtmosphere();
var
    i: integer;
begin
    for i := 0 to starsCount - 1 do
    begin
        Canvas.Pixels[Stars[i].X, Stars[i].Y] := clWhite;
    end;
end;

procedure TMainForm.DrawBackground();
begin
    Canvas.Pen.Color := clGray;
    Canvas.Polyline(Mountains);
    Canvas.Brush.Color := clGray;
    Canvas.FloodFill(10, Height - 10, clGray, fsBorder);
end;

procedure SetTime(time: double);
begin
    clAtmo := LerpColor(clBlack, clWhite, time);
end;

procedure TMainForm.InvalidateMeasures();
var
    R: integer;
begin
    py := Height;
    px := Width div 2;
    R := Height - Rc;
    sun_x0 := px - R;
    sun_y0 := Height;
    ResizeMountains();
end;

procedure TMainForm.GenerateStars();
var
    bx, by, i: integer;
begin
    for i := 0 to starsCount - 1 do
    begin
        bx := random(Width);
        by := random(Height);
        Stars[i] := TPoint.Create(bx, by);
    end;
end;

procedure TMainForm.ResizeMountains();
var
    bx, by, i: integer;
begin
    for i := 0 to mtCount + 1 do
    begin
        if i = 0 then
            bx := -1 // Чтобы убрать левую вершину за границу экрана
        else if i = mtCount + 1 then
            bx := Width + 1 // Чтобы убрать правую вершину за границу экрана
        else
            bx := Trunc(mtRatios[i, 0] * Width);
        by := Trunc(Height - mtRatios[i, 1] * maxHeight);
        Mountains[i] := TPoint.Create(bx, by);
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
    Randomize;
    GenerateStars();
    speed := 0.5;
    InvalidateMeasures();
    SetTime(1);
    alpha := 0;
    sun_x := sun_x0;
    sun_y := sun_y0;
end;

procedure TMainForm.DrawPic();
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
    DrawAtmosphere();
    DrawBackground();
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
    InvalidateMeasures();
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
var
    vx, vy: integer;
    nvx, nvy, ralpha, sina, cosa: Double;
begin
    alpha := alpha + 1 * speed;
    if (alpha > 360) then alpha := alpha - 360;
    vx := sun_x - px;
    vy := sun_y - py;
    ralpha := deg * alpha;
    sina := sin(ralpha);
    cosa := cos(ralpha);
    nvx := vx * cosa - vy * sina;
    nvy := vx * sina + vy * cosa;
    sun_x := Trunc(nvx) + px;
    sun_y := Trunc(nvy) + py;
    SetTime((sina + 1) / 2);
    Repaint;  // Заменил DrawPic на Repaint, т.к иначе картинка очень сильно мерцает
    sun_x := sun_x0;
    sun_y := sun_y0;

end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
    DrawPic();
end;

end.


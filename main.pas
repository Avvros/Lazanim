// Copyright (c) 2022 Александр Жариков
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
        procedure DrawForeground();
        procedure GenerateStars();
        procedure RecalcFarMountains();
        procedure RecalcNearMountains();
        procedure FormResize(Sender: TObject);
        procedure Timer1Timer(Sender: TObject);
        procedure InvalidateMeasures();
    private

    public

    end;

var
    MainForm: TMainForm;

implementation
uses drawing, calc, scene;

var
    sun_x, sun_y, sun_y0, sun_x0, px, py: integer;
    alpha, speed: double;
    clAtmo: TColor;
    FarMountains, LeftMountain, RightMountain: TPointArr;
    Stars: TPRatioArr;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.DrawAtmosphere();
var
    i: integer;
begin
    Canvas.Brush.Color := clAtmo;
    Canvas.Clear;
    for i := 0 to starsCount - 1 do
    begin
        Canvas.Pixels[Trunc(Stars[i, 0] * Width), Trunc(Stars[i, 1] * Height)] := clWhite;
    end;
end;

procedure TMainForm.DrawBackground();
var
    ffs: TPoint; // FloodFill start
begin
    ffs := TPoint.Create(10, Height - 10);
    DrawSolidPolylineObject(Canvas, FarMountains, clFarMt, ffs);
end;

procedure TMainForm.DrawForeground();
var
    ffs: TPoint; // FloodFill start
    moon_x, moon_y: integer;
begin
    Canvas.Brush.Color := clSun;
    DrawCircle(Canvas, sun_x, sun_y, Rc);
    moon_x := Width - sun_x;
    moon_y := 2 * Height - sun_y;
    Canvas.Brush.Color := clMoon;
    DrawCircle(Canvas, moon_x, moon_y, Rc);
    ffs := TPoint.Create(10, Height - 10);
    DrawSolidPolylineObject(Canvas, LeftMountain, clNearMt, ffs);
    ffs := TPoint.Create(Width - 10, Height - 10);
    DrawSolidPolylineObject(Canvas, RightMountain, clNearMt, ffs);
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
    RecalcFarMountains();
    RecalcNearMountains();
end;

procedure TMainForm.GenerateStars();
var
    i: integer;
begin
    SetLength(Stars, starsCount);
    for i := 0 to starsCount - 1 do
    begin
        Stars[i, 0] := random;
        Stars[i, 1] := random;
    end;
end;

procedure TMainForm.RecalcFarMountains();
var
    bx, by, i: integer;
begin
    for i := 0 to FarMtCount + 1 do
    begin
        if i = 0 then
            bx := -1 // Чтобы убрать левую вершину за границу экрана
        else if i = FarMtCount + 1 then
            bx := Width + 1 // Чтобы убрать правую вершину за границу экрана
        else
            bx := Trunc(FarMtRatios[i, 0] * Width);
        by := Trunc(Height - FarMtRatios[i, 1] * maxMtHeight);
        FarMountains[i] := TPoint.Create(bx, by);
    end;
end;

procedure TMainForm.RecalcNearMountains();
var
    bx, by, i: integer;
begin
    for i := 0 to LMtPointsCount + 1 do
    begin
        if i = LMtPointsCount + 1 then
        begin
            by := Height + 1; // Чтобы убрать правую вершину за границу экрана
        end
        else
            by := Trunc(Height - LMtRatios[i, 1] * maxMtHeight);
        if i = 0 then
            bx := -1 // Чтобы убрать левую вершину за границу экрана
        else
            bx := Trunc(LMtRatios[i, 0] * Width);
        LeftMountain[i] := TPoint.Create(bx, by);
        RightMountain[i] := TPoint.Create(Width - bx, by);
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
    Randomize;
    GenerateStars();
    speed := 0.5;
    Setlength(FarMountains, FarMtCount + 2);
    Setlength(LeftMountain, LMtPointsCount + 2);
    Setlength(RightMountain, LMtPointsCount + 2);
    InvalidateMeasures();
    SetTime(1);
    alpha := 0;
    sun_x := sun_x0;
    sun_y := sun_y0;
end;

procedure TMainForm.DrawPic();
begin
    DrawAtmosphere();
    DrawBackground();
    DrawForeground();
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
    ralpha := Deg * alpha;
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


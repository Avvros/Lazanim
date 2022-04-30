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

var
    Form1: TForm1;

implementation

var
    cx, cy, cy0, px, py, alpha: integer;
const
    deg = PI / 180;
    Rc = 20;
    cx0 = Rc;
    //cy0 = 150;

{$R *.lfm}

{ TForm1 }

procedure TForm1.InvalidateMeasures();
var
    R: real;
    dx: integer;
begin
    py := Height;
    px := Width div 2;
    R := Height - Rc;
    dx := cx0 - px;
    cy0 := Trunc(sqrt(R * R - dx * dx) + Height);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    InvalidateMeasures();
    alpha := 0;
    cx := cx0;
    cy := cy0;
end;

procedure TForm1.DrawPic();
begin
    Canvas.Brush.Color := clWhite;
    Canvas.Clear;
    Canvas.Brush.Color := clYellow;
    Canvas.Ellipse(cx - Rc, cy - Rc, cx + Rc, cy + Rc);
    //Canvas.Line(cx, cy, px, py);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
    InvalidateMeasures();
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
    vx, vy: integer;
    nvx, nvy, ralpha: real;
begin
    alpha := alpha + 1;
    vx := cx - px;
    vy := cy - py;
    ralpha := deg * alpha;
    nvx := vx * cos(ralpha) - vy * sin(ralpha);
    nvy := vx * sin(ralpha) + vy * cos(ralpha);
    cx := Trunc(nvx) + px;
    cy := Trunc(nvy) + py;
    DrawPic();
    cx := cx0;
    cy := cy0;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
    DrawPic();
end;

end.


unit FractalScene;

{$mode delphi}

interface

uses
  Codebot.System,
  Codebot.Graphics.Types,
  Codebot.Render.Contexts,
  Codebot.Render.Scenes,
  Codebot.Render.Buffers,
  Codebot.Render.Shaders,
  SysUtils, Classes;

{ TFractalScene }

type
  TFractalScene = class(TScene)
  private
    FAngle: Double;
    FZoomProg: TShaderProgram;
    FTourProg: TShaderProgram;
    FCurrentProg: TShaderProgram;
    FVerts: TFlatVertexBuffer;
    FDrag: TRectI;
    FMoving: Boolean;
    FMoveStart: Double;
    FMoveEnd: Double;
    FX, FY, FZ, FSX, FSY, FSZ, FDX, FDY, FDZ: Double;
    FOnModeChange: TNotifyEvent;
    function GetIsTour: Boolean;
    procedure SetZoom(Value: Double);
  public
    procedure Initialize; override;
    procedure Logic; override;
    procedure Render; override;
    procedure ClientToFractal(var X, Y: Double);
    procedure Drag(const Rect: TRectI);
    procedure Pan(X, Y: Double);
    procedure MoveTo(const X, Y, Z: Double);
    property Angle: Double read FAngle write FAngle;
    property Zoom: Double read FZ write SetZoom;
    property X: Double read FX write FX;
    property Y: Double read FY write FY;
    property IsTour: Boolean read GetIsTour;
    property OnModeChange: TNotifyEvent read FOnModeChange write FOnModeChange;
  end;

implementation

{ TFractalScene }

procedure TFractalScene.SetZoom(Value: Double);
begin
  if Value < 0.1 then
    Value := 0.1;
  FZ := Value;
end;

function TFractalScene.GetIsTour: Boolean;
begin
  Result := FCurrentProg = FTourProg;
end;

procedure TFractalScene.Initialize;
var
  S: string;
begin
  Context.SetClearColor(1, 0, 0, 0);
  FZ := 1;
  FZoomProg := TShaderProgram.CreateFromFile('zoom-hi');
  FZoomProg.Name := 'zoom';
  if not FZoomProg.Valid then
  begin
    FZoomProg.Free;
    FZoomProg := TShaderProgram.CreateFromFile('zoom-lo');
    FZoomProg.Name := 'zoom';
    if not FZoomProg.Valid then
    begin
      S := FZoomProg.ErrorString;
      FZoomProg.Free;
      FZoomProg := nil;
      raise EOpenGLError.Create(S);
    end;
  end;
  FTourProg := TShaderProgram.CreateFromFile('tour');
  FTourProg.Name := 'tour';
  if not FTourProg.Valid then
  begin
    FZoomProg.Free;
    FZoomProg := nil;
    S := FTourProg.ErrorString;
    FTourProg.Free;
    FTourProg := nil;
    raise EOpenGLError.Create(S);
  end;
  FCurrentProg := FZoomProg;
  FVerts := TFlatVertexBuffer.Create(3);
  FVerts.Name := 'triangle';
  FVerts.SetProgram(-1);
  FVerts.Add(-1, 1);
  FVerts.Add(-1, -1);
  FVerts.Add(1, -1);
  FVerts.Add(1, 1);
end;

procedure TFractalScene.Logic;
var
  P: TShaderProgram;
begin
  if FCurrentProg = nil then
    Exit;
  P := FCurrentProg;
  if IsKeyDown(VK_Q) then
    P := FZoomProg
  else if IsKeyDown(VK_W) then
    P := FTourProg;
  if P <> FCurrentProg then
  begin
    FCurrentProg := P;
    if Assigned(FOnModeChange) then
      FOnModeChange(Self);
  end;
end;

const
  MoveToTime = 0.5;

procedure TFractalScene.Render;
var
  R, I: Double;
begin
  Context.Clear;
  if FCurrentProg = nil then
    Exit;
  FCurrentProg.Push;
  Context.SetUniform('resolution', Width, Height);
  Context.SetUniform('time', Time);
  if not IsTour then
  begin
    if FMoving then
      if Time < FMoveEnd then
      begin
        R := (Time - FMoveStart) / MoveToTime;
        if R < 0.5 then
        begin
          R := R / 0.5;
          I := 1 - R;
          FX := FSX * I + FDX * R;
          FY := FSY * I + FDY * R;
        end
        else
        begin
          R := (R - 0.5) / 0.5;
          I := 1 - R;
          FX := FDX;
          FY := FDY;
          FZ := FSZ * I + FDZ * R;
        end;
      end
      else
      begin
        FMoving := False;
        FX := FDX; FY := FDY; FZ := FDZ;
      end;
    Context.SetUniform('angle', Sin(Time / 4) / 2);
    Context.SetUniform('center', FX, FY);
    Context.SetUniform('zoom', FZ);
    Context.SetUniform('drag', (FDrag.Width > 0) and (FDrag.Height > 0));
    Context.SetUniform('rect', FDrag.X, FDrag.Y, FDrag.Right, FDrag.Bottom);
  end;
  FVerts.Draw(vertTriangleFan, 0, 4);
  FCurrentProg.Pop;
end;

procedure TFractalScene.ClientToFractal(var X, Y: Double);
begin
  X := (X - Width / 2) / Width * 3 / FZ;
  Y := (Y - Height / 2) / Width * -3 / FZ;
  X := X + FX;
  Y := Y + FY;
end;

procedure TFractalScene.Drag(const Rect: TRectI);
begin
  if IsTour then
    Exit;
  if FMoving then
    Exit;
  if Rect.Left < Rect.Right then
    FDrag.Left := Rect.Left
  else
    FDrag.Left := Rect.Right;
  FDrag.Width := Abs(Rect.Width);
  if Rect.Top < Rect.Bottom then
    FDrag.Top := Rect.Top
  else
    FDrag.Top := Rect.Bottom;
  FDrag.Height := Abs(Rect.Height);
end;

procedure TFractalScene.Pan(X, Y: Double);
begin
  if IsTour then
    Exit;
  if FMoving then
    Exit;
  X := X  / Width * 3 / FZ;
  Y := Y / Width * -3 / FZ;
  FX := FX + X;
  FY := FY + Y;
end;

procedure TFractalScene.MoveTo(const X, Y, Z: Double);
begin
  if IsTour then
    Exit;
  if FMoving then
    Exit;
  FMoving := True;
  FDrag.Left := 0;
  FDrag.Top := 0;
  FDrag.Width := 0;
  FDrag.Height := 0;
  FMoveStart := Time;
  FMoveEnd := FMoveStart + MoveToTime;
  FSX := FX; FSY := FY; FSZ := FZ;
  FDX := X; FDY := Y; FDZ := Z;
  if (FSX = FDX) and (FSY = FDY) then
  begin
    FMoveStart := FMoveStart - MoveToTime / 2;
    FMoveEnd := FMoveEnd - MoveToTime / 2;
  end;
end;

end.


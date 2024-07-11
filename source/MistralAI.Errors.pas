unit MistralAI.Errors;

interface

uses
  REST.Json.Types;

type
  TError = class
  private
    [JsonNameAttribute('message')]
    FMessage: string;
    [JsonNameAttribute('detail')]
    FDetail: string;
    [JsonNameAttribute('request_id')]
    FRequestID: string;
  public
    property Message: string read FMessage write FMessage;
    property Detail: string read FDetail write FDetail;
    property RequestID: string read FRequestID write FRequestID;
  end;

  TDetail = class
  private
    [JsonNameAttribute('loc')]
    FLoc: TArray<string>;
    [JsonNameAttribute('msg')]
    FMsg: string;
    [JsonNameAttribute('type')]
    FType: string;
  public
    property Loc: TArray<string> read FLoc write FLoc;
    property Msg: string read FMsg write FMsg;
    property &Type: string read FType write FType;
  end;

  TValidationError = class
  private
    [JsonNameAttribute('detail')]
    FDetail: TArray<TDetail>;
  public
    property Detail: TArray<TDetail> read FDetail write FDetail;
    destructor Destroy; override;
  end;

implementation

{ TValidationError }

destructor TValidationError.Destroy;
begin
  for var Item in Detail do
    Item.Free;
  inherited;
end;

end.

unit MistralAI.Errors;

{-------------------------------------------------------------------------------

      Github repository : https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  REST.Json.Types;

type
  TErrorCore = class abstract
  end;

  TError = class(TErrorCore)
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

  TCTX = class
  private
    [JsonNameAttribute('le')]
    FLe: Double;
  public
    property Le: Double read FLe write FLe;
  end;

  TDetail = class
  private
    [JsonNameAttribute('loc')]
    FLoc: TArray<string>;
    [JsonNameAttribute('msg')]
    FMsg: string;
    [JsonNameAttribute('type')]
    FType: string;
    [JsonNameAttribute('input')]
    FInput: string;
    [JsonNameAttribute('ctx')]
    FCTX: TCTX;
  public
    property Loc: TArray<string> read FLoc write FLoc;
    property Msg: string read FMsg write FMsg;
    property &Type: string read FType write FType;
    property Input: string read FInput write FInput;
    property CTX: TCTX read FCTX write FCTX;
    destructor Destroy; override;
  end;

  TMessage422 = class
  private
    [JsonNameAttribute('detail')]
    FDetail: TArray<TDetail>;
  public
    property Detail: TArray<TDetail> read FDetail write FDetail;
    destructor Destroy; override;
  end;

  TError422 = class(TErrorCore)
  private
     [JsonNameAttribute('object')]
     FObject: string;
     [JsonNameAttribute('message')]    // NOTE : Should't exists
     FMessage: TMessage422;
     [JsonNameAttribute('type')]
     FType: string;
     [JsonNameAttribute('param')]
     FParam: string;
     [JsonNameAttribute('code')]
     FCode: string;
     [JsonNameAttribute('detail')]
     FDetail: TArray<TDetail>;
  public
    property &Object: string read FObject write FObject;
    {--- "Message" Property that should not exist according to API documentation.
         But some 422 errors use this property.}
    property Message: TMessage422 read FMessage write FMessage;
    property &Type: string read FType write FType;
    property Param: string read FParam write FParam;
    property Code: string read FCode write FCode;
    property Detail: TArray<TDetail> read FDetail write FDetail;
    destructor Destroy; override;
  end;

implementation

{ TError422 }

destructor TError422.Destroy;
begin
  if Assigned(FMessage) then
    FMessage.Free;
  if Assigned(FDetail) then
    for var Item in FDetail do
      Item.Free;
  inherited;
end;

{ TDetail }

destructor TDetail.Destroy;
begin
  if Assigned(FCTX) then
    FCTX.Free;
  inherited;
end;

{ TMessage422 }

destructor TMessage422.Destroy;
begin
  for var Item in Detail do
    Item.Free;
  inherited;
end;

end.

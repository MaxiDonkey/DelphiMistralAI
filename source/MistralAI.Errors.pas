unit MistralAI.Errors;

interface

type
  TError = class
  private
    FMessage: string;
    FRequest_id: string;
  public
    property Message: string read FMessage write FMessage;
    property Request_id: string read FRequest_id write FRequest_id;
  end;

implementation

end.

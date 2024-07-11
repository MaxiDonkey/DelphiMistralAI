unit MistralAI.Models;

interface

uses
  System.SysUtils, REST.Json.Types, MistralAI.API.Params, MistralAI.API;

type
  /// <summary>
  /// Describes an MistralAI model offering that can be used with the API.
  /// </summary>
  TModel = class
  private
    [JsonNameAttribute('id')]
    FId: string;
    [JsonNameAttribute('object')]
    FObject: string;
    [JsonNameAttribute('created')]
    FCreated: Int64;
    [JsonNameAttribute('owned_by')]
    FOwned_by: string;
  public
    /// <summary>
    /// The model identifier, which can be referenced in the API endpoints
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// The object type, which is always "model"
    /// </summary>
    property &Object: string read FObject write FObject;
    /// <summary>
    /// The Unix timestamp (in seconds) when the model was created
    /// </summary>
    property Created: Int64 read FCreated write FCreated;
    /// <summary>
    /// The owner of the model
    /// </summary>
    property OwnedBy: string read FOwned_by write FOwned_by;
  end;

  /// <summary>
  /// Lists the currently available models, and provides basic information about each one
  /// such as the owner and availability.
  /// </summary>
  TModels = class
  private
    [JsonNameAttribute('object')]
    FObject: string;
    [JsonNameAttribute('data')]
    FData: TArray<TModel>;
  public
    /// <summary>
    /// Title of model
    /// </summary>
    property &Object: string read FObject write FObject;
    /// <summary>
    /// Array of objects (Model)
    /// </summary>
    property Data: TArray<TModel> read FData write FData;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Class managing the data returned upon successful completion of a request to delete a fine-tuned model
  /// </summary>
  TModelDeletion = class
  private
    [JsonNameAttribute('id')]
    FId: string;
    [JsonNameAttribute('object')]
    FObject: string;
    [JsonNameAttribute('deleted')]
    FDeleted: Boolean;
  public
    /// <summary>
    /// The ID of the deleted model
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// Default: "model"
    /// The object type that was deleted
    /// </summary>
    property &Object: string read FObject write FObject;
    /// <summary>
    /// The deletion status
    /// </summary>
    property Deleted: Boolean read FDeleted write FDeleted;
  end;

  TModelsRoute = class(TMistralAIAPIRoute)
  public
    /// <summary>
    /// Lists the currently available models
    /// </summary>
    function List: TModels;
    /// <summary>
    /// Delete a fine-tuned model
    /// </summary>
    /// <param name="ModelId"> The ID of the fine-tuned model to be deleted </param>
    function Delete(const ModelId: string): TModelDeletion;
  end;

implementation

{ TModels }

destructor TModels.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TModelsRoute }

function TModelsRoute.Delete(const ModelId: string): TModelDeletion;
begin
  Result := API.Delete<TModelDeletion>(Format('models/%s', [ModelId]));
end;

function TModelsRoute.List: TModels;
begin
  Result := API.Get<TModels>('models');
end;

end.

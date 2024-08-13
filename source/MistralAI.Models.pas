unit MistralAI.Models;

interface

uses
  System.SysUtils, System.Classes, REST.Json.Types, MistralAI.API.Params,
  MistralAI.API;

type
  TModelParams = class(TJSONParam)
    /// <summary>
    /// The New Name value of the fine-tuned model to update
    /// Warning: Not to be confused with the model ID which does not move
    /// </summary>
    function Name(const Value: string): TModelParams;
    /// <summary>
    /// The new description of the fine-tuned model to update
    /// </summary>
    function Description(const Value: string): TModelParams;
  end;

  TCapabilities = class
  private
    [JsonNameAttribute('completion_chat')]
    FCompletionChat: Boolean;
    [JsonNameAttribute('completion_fim')]
    FCompletionFim: Boolean;
    [JsonNameAttribute('function_calling')]
    FFunctionCalling: Boolean;
    [JsonNameAttribute('fine_tuning')]
    FFineTuning: Boolean;
  public
    property CompletionChat: Boolean read FCompletionChat write FCompletionChat;
    property CompletionFim: Boolean read FCompletionFim write FCompletionFim;
    property FunctionCalling: Boolean read FFunctionCalling write FFunctionCalling;
    property FineTuning: Boolean read FFineTuning write FFineTuning;
  end;

  /// <summary>
  /// Describes an MistralAI model offering that can be used with the API.
  /// </summary>
  TCoreModel = class
  private
    [JsonNameAttribute('id')]
    FId: string;
    [JsonNameAttribute('object')]
    FObject: string;
    [JsonNameAttribute('created')]
    FCreated: Int64;
    [JsonNameAttribute('owned_by')]
    FOwned_by: string;
    [JsonNameAttribute('root')]
    FRoot: string;
    [JsonNameAttribute('archived')]
    FArchived: Boolean;
    [JsonNameAttribute('name')]
    FName: string;
    [JsonNameAttribute('description')]
    FDescription: string;
    [JsonNameAttribute('capabilities')]
    FCapabilities: TCapabilities;
    [JsonNameAttribute('max_context_length')]
    FMaxContextLength: Int64;
    [JsonNameAttribute('aliases')]
    FAliases: TArray<string>;
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
    /// <summary>
    /// Root of the model
    /// </summary>
    property Root: string read FRoot write FRoot;
    /// <summary>
    /// Returns True if the model is archived
    /// </summary>
    property Archived: Boolean read FArchived write FArchived;
    /// <summary>
    /// Name of the modèle for fined-tuned model.
    /// </summary>
    property Name: string read FName write FName;
    /// <summary>
    /// Description of the model
    /// </summary>
    property Description: string read FDescription write FDescription;
    /// <summary>
    /// Return the capabilities of the model
    /// </summary>
    property Capabilities: TCapabilities read FCapabilities write FCapabilities;
    /// <summary>
    /// Returns the max_context_length valur of the model
    /// </summary>
    property MaxContextLength: Int64 read FMaxContextLength write FMaxContextLength;
    /// <summary>
    /// Resturns the aliases of the model in an array of strings
    /// </summary>
    property Aliases: TArray<string> read FAliases write FAliases;

    destructor Destroy; override;
  end;

  TModel = class(TCoreModel)
  private
    [JsonNameAttribute('deprecation')]
    FDeprecation: string;
  public
    /// <summary>
    /// Returns the date of model deprecation like one string
    /// </summary>
    property Deprecation: string read FDeprecation write FDeprecation;
  end;

  TFineTunedModel = class(TCoreModel)
  private
    [JsonNameAttribute('job')]
    FJob: string;
  public
    property Job: string read FJob write FJob;
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
    /// The ID of the fine-tuned model to be delete
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// Default: "model"
    /// The object type that was deleted
    /// </summary>
    property &Object: string read FObject write FObject;
    /// <summary>
    /// The deletion flag
    /// </summary>
    property Deleted: Boolean read FDeleted write FDeleted;
  end;

  TArchivingdModel = class
  private
    [JsonNameAttribute('id')]
    FId: string;
    [JsonNameAttribute('object')]
    FObject: string;
    [JsonNameAttribute('archived')]
    FArchived: Boolean;
  public
    /// <summary>
    /// The ID of the fine-tuned model to be archive or unarchive
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// Default: "model"
    /// The object type that was archived or unarchived
    /// </summary>
    property &Object: string read FObject write FObject;
    /// <summary>
    /// The archiving flag
    /// </summary>
    property Archived: Boolean read FArchived write FArchived;
  end;

  TModelsRoute = class(TMistralAIAPIRoute)
  public
    /// <summary>
    /// Lists the currently available models
    /// </summary>
    function List: TModels;
    /// <summary>
    /// Delete one fine-tuned model
    /// </summary>
    /// <param name="ModelId"> The ID of the fine-tuned model to be deleted </param>
    function Delete(const ModelId: string): TModelDeletion;
    /// <summary>
    /// Retrieve one model
    /// </summary>
    /// <param name="ModelId"> The ID of the model to be retrieved </param>
    function Retrieve(const ModelId: string): TModel;
    /// <summary>
    /// Update the name or the description of one fine-tuned model
    /// </summary>
    function Update(const ModelId: string; ParamProc: TProc<TModelParams>): TFineTunedModel;
    /// <summary>
    /// Archive a fine-tuned model
    /// </summary>
    function Archive(const ModelId: string): TArchivingdModel;
    /// <summary>
    /// Un-archive a fine-tuned model
    /// </summary>
    function Unarchive(const ModelId: string): TArchivingdModel;
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

function TModelsRoute.Archive(const ModelId: string): TArchivingdModel;
begin
  Result := API.Post<TArchivingdModel>(Format('fine_tuning/models/%s/archive', [ModelId]));
end;

function TModelsRoute.Delete(const ModelId: string): TModelDeletion;
begin
  Result := API.Delete<TModelDeletion>(Format('models/%s', [ModelId]));
end;

function TModelsRoute.List: TModels;
begin
  Result := API.Get<TModels>('models');
end;

function TModelsRoute.Retrieve(const ModelId: string): TModel;
begin
  Result := API.Get<TModel>(Format('models/%s', [ModelId]));
end;

function TModelsRoute.Unarchive(const ModelId: string): TArchivingdModel;
begin
  Result := API.Delete<TArchivingdModel>(Format('fine_tuning/models/%s/archive', [ModelId]));
end;

function TModelsRoute.Update(const ModelId: string;
  ParamProc: TProc<TModelParams>): TFineTunedModel;
begin
  Result := API.Post<TFineTunedModel, TModelParams>(Format('fine_tuning/models/%s', [ModelId]), ParamProc);
end;

{ TCoreModel }

destructor TCoreModel.Destroy;
begin
  if Assigned(FCapabilities) then
    FCapabilities.Free;
  inherited;
end;

{ TModelParams }

function TModelParams.Description(const Value: string): TModelParams;
begin
  Result := TModelParams(Add('description', Value));
end;

function TModelParams.Name(const Value: string): TModelParams;
begin
  Result := TModelParams(Add('name', Value));
end;


end.

unit MistralAI.FineTunings;

{
+------------------------------------------------------------------------------+
|                                                                              |
|  **Important Notice**                                                        |
|                                                                              |
| As of June 2024, the only fine-tunable models at Mistral are                 |
|        "open-mistral-7b" and "mistral-small-latest".                         |
|                                                                              |
| To monitor and track the progress of fine-tuning using Wandb.ai tools,       |
| you are required to register on the Wandb website at                         |
| https://wandb.ai/site].                                                      |
| Registration is necessary to obtain the API key needed for "job" integration |
| (TJobIntegrationsParam).                                                     |
|                                                                              |
+------------------------------------------------------------------------------+
}

interface

uses
  System.Classes, System.SysUtils, REST.JsonReflect, System.JSON, REST.Json.Types,
  MistralAI.API.Params, MistralAI.API;

type
  /// <summary>
  /// Enum of the different statuses for fine tuning job
  /// </summary>
  TFineTuningJobStatus = (
    /// <summary>
    /// The job is queued, it is waiting to start
    /// </summary>
    Queued,
    /// <summary>
    /// The job is started
    /// </summary>
    Started,
    /// <summary>
    /// The job is running
    /// </summary>
    Running,
    /// <summary>
    /// The job ended has failed
    /// </summary>
    Failed,
    /// <summary>
    /// The job ended successfully
    /// </summary>
    Success,
    /// <summary>
    /// The job has been cancelled
    /// </summary>
    Cancelled,
    /// <summary>
    /// A cancellation request has been made, the job is awaiting cancellation
    /// </summary>
    CancellationRequested);

  TFineTuningJobStatusHelper = record helper for TFineTuningJobStatus
    function ToString: string;
    class function Create(const Value: string): TFineTuningJobStatus; static;
  end;

  TFineTuningJobStatusInterceptor = class(TJSONInterceptorStringToString)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TFineTuningDataObjectKind = (
    Job
  );

  TFineTuningDataObjectKindHelper = record helper for TFineTuningDataObjectKind
    function ToString: string;
    class function Create(const Value: string): TFineTuningDataObjectKind; static;
  end;

  TFineTuningDataObjectKindInterceptor = class(TJSONInterceptorStringToString)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// Type of platform with which to integrate monitoring information for fine tuning operations
  /// </summary>
  TFineTuningIntegrationType = (
    /// <summary>
    /// See "Weights and Biases" solutions at the web site "https://wandb.ai/site"
    /// </summary>
    Wandb
  );

  TFineTuningIntegrationTypeHelper = record helper for TFineTuningIntegrationType
    function ToString: string;
    class function Create(const Value: string): TFineTuningIntegrationType; static;
  end;

  TFineTuningIntegrationTypeInterceptor = class(TJSONInterceptorStringToString)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TFineTuningObjectKind = (
    List
  );

  TFineTuningObjectKindHelper = record helper for TFineTuningObjectKind
    function ToString: string;
    class function Create(const Value: string): TFineTuningObjectKind; static;
  end;

  TFineTuningObjectKindInterceptor = class(TJSONInterceptorStringToString)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TFineTuningJobListParams = class(TJSONParam)
    /// <summary>
    /// The page number of the results to be returned
    /// </summary>
    /// <remarks> Default: 0 </remarks>
    function Page(const Value: Int64): TFineTuningJobListParams;
    /// <summary>
    /// The number of items to return per page
    /// </summary>
    /// <remarks> Default: 100 </remarks>
    function PageSize(const Value: Int64): TFineTuningJobListParams;
    /// <summary>
    /// The model name used for fine-tuning to filter on. When set, the other results are not displayed
    /// </summary>
    function Model(const Value: string): TFineTuningJobListParams;
    /// <summary>
    /// The current job state to filter on. When set, the other results are not displayed
    /// </summary>
    function Status(const Value: TFineTuningJobStatus): TFineTuningJobListParams;
    /// <summary>
    /// The date/time to filter on. When set, the results for previous creation times are not displayed
    /// </summary>
    function CreatedAfter(const Value: string): TFineTuningJobListParams;
    /// <summary>
    /// When set, only return results for jobs created by the API caller. Other results are not displayed
    /// </summary>
    /// <remarks> Default: false </remarks>
    function CreatedByMe(const Value: Boolean = True): TFineTuningJobListParams;
    /// <summary>
    /// The Weights and Biases project to filter on. When set, the other results are not displayed
    /// </summary>
    function WandbProject(const Value: string): TFineTuningJobListParams;
    /// <summary>
    /// The Weight and Biases run name to filter on. When set, the other results are not displayed
    /// </summary>
    function WandbName(const Value: string): TFineTuningJobListParams;
    /// <summary>
    /// The model suffix to filter on. When set, the other results are not displayed
    /// </summary>
    function Suffix(const Value: string): TFineTuningJobListParams;
  end;

  THyperparametersParam = record
  const
    LearningRateDefault = 0.0001;
  private
     FTrainingSteps: Int64;
     FLearningRate: Extended;
    procedure SetTrainingSteps(const Value: Int64);
    procedure SetLearningRate(const Value: Extended);
  public
    /// <summary>
    /// The number of training steps to perform. A training step refers to a single update of the model
    /// weights during the fine-tuning process. This update is typically calculated using a batch of
    /// samples from the training dataset
    /// </summary>
    /// <param name="Value">
    /// greater or equal to 1
    /// </param>
    /// <remarks> is required </remarks>
    property TrainingSteps: Int64 read FTrainingSteps write SetTrainingSteps;
    /// <summary>
    /// A parameter describing how much to adjust the pre-trained model's weights in response to
    /// the estimated error each time the weights are updated during the fine-tuning proces
    /// </summary>
    /// <param name="Value">
    /// must be in the range [1e-8..1]
    /// </param>
    /// <remarks> Default value: 0.0001 </remarks>
    property LearningRate: Extended read FLearningRate write SetLearningRate;

    class function Create(const ATrainingSteps: Int64 = 1; const ALearningRate: Extended = LearningRateDefault): THyperparametersParam; static;
  end;

  TJobIntegrationsParam = record
  private
    FType: TFineTuningIntegrationType;
    FProject: string;
    FName: string;
    FApiKey: string;
  public
    /// <summary>
    /// Default: "wandb"
    /// Value: "wandb"
    /// </summary>
    /// <remarks>
    /// A single value is currently defined. Values could be added in the future
    /// </remarks>
    property &Type: TFineTuningIntegrationType read FType write FType;
    /// <summary>
    /// The name of the project that the new run will be created under
    /// </summary>
    /// <remarks> is required </remarks>
    property Project: string read FProject write FProject;
    /// <summary>
    /// A display name to set for the run. If not set, will use the job ID as the name
    /// </summary>
    property Name: string read FName write FName;
    /// <summary>
    /// The WandB API key to use for authentication
    /// </summary>
    /// <remarks> is required </remarks>
    property ApiKey: string read FApiKey write FApiKey;

    class function Create(const AType: TFineTuningIntegrationType; const Project, Name, ApiKey: string): TJobIntegrationsParam; static;
    class function Wandb(const Project, Name, ApiKey: string): TJobIntegrationsParam; static;
  end;

  TFineTuningJobParams = class(TJSONParam)
  public
    /// <summary>
    /// Fine Tuneable Models : Enum "open-mistral-7b", "mistral-small-latest"
    /// The name of the model to fine-tune
    /// </summary>
    /// <remarks> is required </remarks>
    function Model(const Value: string): TFineTuningJobParams;
    /// <summary>
    /// A list containing the IDs of uploaded files that contain training data
    /// </summary>
    /// <remarks> is required </remarks>
    function TrainingFiles(const Value: TArray<string>): TFineTuningJobParams;
    /// <summary>
    /// A list containing the IDs of uploaded files that contain validation data
    /// </summary>
    /// <remarks>
    /// If you provide these files, the data is used to generate validation metrics periodically
    /// during fine-tuning. These metrics can be viewed in checkpoints when getting the status of
    /// a running fine-tuning job
    /// </remarks>
    function ValidationFiles(const Value: TArray<string>): TFineTuningJobParams;
    /// <summary>
    /// The fine-tuning hyperparameter settings used in a fine-tune job
    /// </summary>
    /// <remarks> is required </remarks>
    function Hyperparameters(Value: THyperparametersParam): TFineTuningJobParams; overload;
    /// <summary>
    /// A string that will be added to your fine-tuning model name. For example, a suffix of "my-great-model"
    /// would produce a model name like ft:open-mistral-7b:my-great-model:xxx...
    /// </summary>
    /// <param name="Value">
    /// less than 18 characters
    /// </param>
    function Suffix(const Value: string): TFineTuningJobParams;
    /// <summary>
    /// A list of integrations to enable for your fine-tuning job
    /// </summary>
    function Integrations(const Value: TArray<TJobIntegrationsParam>): TFineTuningJobParams;
  end;

  TJobOutHyperparameters = class
  private
    [JsonNameAttribute('training_steps')]
    FTrainingSteps: Int64;
    [JsonNameAttribute('learning_rate')]
    FLearningRate: Extended;
  public
    /// <summary>
    /// The number of training steps to perform. A training step refers to a single update of the model weights
    /// during the fine-tuning process. This update is typically calculated using a batch of samples from the
    /// training dataset
    /// </summary>
    /// <remarks>
    /// Returns an integer value >= 1. Value is required
    /// </remarks>
    property TrainingSteps: Int64 read FTrainingSteps write FTrainingSteps;
    /// <summary>
    /// A parameter describing how much to adjust the pre-trained model's weights in response to the estimated
    /// error each time the weights are updated during the fine-tuning process
    /// </summary>
    /// <remarks>
    /// Extended value in [ 1e-8 .. 1 ]
    /// </remarks>
    property LearningRate: Extended read FLearningRate write FLearningRate;
  end;

  TJobOutIntegrations = class
  private
    [JsonReflectAttribute(ctString, rtString, TFineTuningIntegrationTypeInterceptor)]
    FType: TFineTuningIntegrationType;
    [JsonNameAttribute('project')]
    FProject: string;
    [JsonNameAttribute('name')]
    FName: string;
  public
    /// <summary>
    /// Default: "wandb"
    /// Value: "wandb"
    /// </summary>
    /// <remarks>
    /// A single value is currently defined. Values could be added in the future
    /// </remarks>
    property &Type: TFineTuningIntegrationType read FType write FType;
    /// <summary>
    /// The name of the project that the new run will be created under
    /// </summary>
    /// <remarks> Value is required </remarks>
    property Project: string read FProject write FProject;
    /// <summary>
    /// A display name to set for the run. If not set, will use the job ID as the name
    /// </summary>
    property Name: string read FName write FName;
  end;

  TJobOut = class
  private
    [JsonNameAttribute('id')]
    FId: string;
    [JsonNameAttribute('hyperparameters')]
    FHyperparameters: TJobOutHyperparameters;
    [JsonNameAttribute('model')]
    FModel: string;
    [JsonReflectAttribute(ctString, rtString, TFineTuningJobStatusInterceptor)]
    FStatus: TFineTuningJobStatus;
    [JsonNameAttribute('job_type')]
    FJobType: string;
    [JsonNameAttribute('created_at')]
    FCreatedAt: Int64;
    [JsonNameAttribute('modified_at')]
    FModifiedAt: Int64;
    [JsonNameAttribute('training_files')]
    FTrainingFiles: TArray<string>;
    [JsonNameAttribute('validation_files')]
    FValidationFiles: TArray<string>;
    [JsonReflectAttribute(ctString, rtString, TFineTuningDataObjectKindInterceptor)]
    FObject: TFineTuningDataObjectKind;
    [JsonNameAttribute('fine_Tuning_model')]
    FFineTuningModel: string;
    [JsonNameAttribute('integrations')]
    FIntegrations: TArray<TJobOutIntegrations>;
  public
    /// <summary>
    /// The ID of the job
    /// </summary>
    /// <remarks> Value is required </remarks>
    property Id: string read FId write FId;
    /// <summary>
    /// The fine-tuning hyperparameter settings used in a fine-tune job
    /// </summary>
    /// <remarks> Value is required </remarks>
    property Hyperparameters: TJobOutHyperparameters read FHyperparameters write FHyperparameters;
    /// <summary>
    /// The name of the model to fine-tune
    /// </summary>
    /// <remarks> Value is required </remarks>
    property Model: string read FModel write FModel;
    /// <summary>
    /// The current status of the fine-tuning job
    /// A single value is currently defined. Values could be added in the future
    /// </summary>
    /// <remarks> Value is required. </remarks>
    property Status: TFineTuningJobStatus read FStatus write FStatus;
    /// <summary>
    /// The type of job (FT for fine-tuning)
    /// </summary>
    /// <remarks> Value is required </remarks>
    property JobType: string read FJobType write FJobType;
    /// <summary>
    /// The UNIX timestamp (in seconds) for when the fine-tuning job was created
    /// </summary>
    /// <remarks> Value is required </remarks>
    property CreatedAt: Int64 read FCreatedAt write FCreatedAt;
    /// <summary>
    /// The UNIX timestamp (in seconds) for when the fine-tuning job was last modified
    /// </summary>
    /// <remarks> Value is required </remarks>
    property ModifiedAt: Int64 read FModifiedAt write FModifiedAt;
    /// <summary>
    /// A list containing the IDs of uploaded files that contain training data
    /// </summary>
    /// <remarks> Value is required </remarks>
    property TrainingFiles: TArray<string> read FTrainingFiles write FTrainingFiles;
    /// <summary>
    /// A list containing the IDs of uploaded files that contain validation data
    /// </summary>
    property ValidationFiles: TArray<string> read FValidationFiles write FValidationFiles;
    /// <summary>
    /// Default: "job"
    /// Value: "job"
    /// </summary>
    /// <remarks>
    /// A single value is currently defined. Values could be added in the future
    /// </remarks>
    property &Object: TFineTuningDataObjectKind read FObject write FObject;
    /// <summary>
    /// The name of the fine-Tuning model that is being created. The value will be null
    /// if the fine-tuning job is still running.
    /// </summary>
    property FineTuningModel: string read FFineTuningModel write FFineTuningModel;
    /// <summary>
    /// A list of integrations enabled for your fine-tuning job
    /// </summary>
    property Integrations: TArray<TJobOutIntegrations> read FIntegrations write FIntegrations;
    Destructor Destroy; override;
  end;

  TListFineTuningJobs = class
  private
    [JsonNameAttribute('data')]
    FData: TArray<TJobOut>;
    [JsonReflectAttribute(ctString, rtString, TFineTuningObjectKindInterceptor)]
    FObject: TFineTuningObjectKind;
  public
    /// <summary>
    /// Table of common jobs
    /// </summary>
    property Data: TArray<TJobOut> read FData write FData;
    /// <summary>
    /// Default: "list"
    /// Value: "list"
    /// </summary>
    /// <remarks>
    /// A single value is currently defined. Values could be added in the future
    /// </remarks>
    property &Object: TFineTuningObjectKind read FObject write FObject;
    Destructor Destroy; override;
  end;

  JobMetadata = class
  private
    [JsonNameAttribute('training_steps')]
    FTrainingSteps: Int64;
    [JsonNameAttribute('train_tokens_per_step')]
    FTrainTokensPerStep: Int64;
    [JsonNameAttribute('data_tokens')]
    FDataTokens: Int64;
    [JsonNameAttribute('train_tokens')]
    FTrainTokens: Int64;
    [JsonNameAttribute('epochs')]
    FEpochs: Extended;
    [JsonNameAttribute('expected_duration_seconds')]
    FExpectedDurationSeconds: Int64;
  public
    /// <summary>
    /// The number of training steps to perform. A training step refers to a single update of the model
    /// weights during the fine-tuning process. This update is typically calculated using a batch of
    /// samples from the training dataset
    /// </summary>
    property TrainingSteps: Int64 read FTrainingSteps write FTrainingSteps;
    /// <summary>
    /// The number of tokens consumed by one training step
    /// </summary>
    property TrainTokensPerStep: Int64 read FTrainTokensPerStep write FTrainTokensPerStep;
    /// <summary>
    /// The total number of tokens in the training dataset
    /// </summary>
    property DataTokens: Int64 read FDataTokens write FDataTokens;
    /// <summary>
    /// The total number of tokens used during the fine-tuning process
    /// </summary>
    property TrainTokens: Int64 read FTrainTokens write FTrainTokens;
    /// <summary>
    /// The number of complete passes through the entire training dataset
    /// </summary>
    property Epochs: Extended read FEpochs write FEpochs;
    /// <summary>
    /// The approximated time (in seconds) for the fine-tuning process to complete
    /// </summary>
    property ExpectedDurationSeconds: Int64 read FExpectedDurationSeconds write FExpectedDurationSeconds;
  end;

  TJobOutEvent = class
  private
    [JsonNameAttribute('name')]
    FName: string;
    [JsonReflectAttribute(ctString, rtString, TFineTuningJobStatusInterceptor)]
    FData: TFineTuningJobStatus;
    [JsonNameAttribute('created_at')]
    FCreatedAt: int64;
  public
    /// <summary>
    /// The name of the eventt
    /// </summary>
    /// <remarks>
    /// Value is required
    /// </remarks>
    property Name: string read FName write FName;
    /// <summary>
    /// The status of the fine-tuning job at the time of the event
    /// </summary>
    /// <remarks>
    /// Enum: "QUEUED" "STARTED" "RUNNING" "FAILED" "SUCCESS" "CANCELLED" "CANCELLATION_REQUESTED"
    /// </remarks>
    property Data: TFineTuningJobStatus read FData write FData;
    /// <summary>
    /// The UNIX timestamp (in seconds) of the event
    /// </summary>
    /// <remarks>
    /// Value is required
    /// </remarks>
    property CreatedAt: int64 read FCreatedAt write FCreatedAt;
  end;

  TJobOutMetrics = class
  private
    [JsonNameAttribute('train_loss')]
    FTrainLoss: Extended;
    [JsonNameAttribute('valid_loss')]
    FValidLoss: Extended;
    [JsonNameAttribute('valid_mean_token_accuracy')]
    FValidMeanTokenAccuracy: Extended;
  public
    /// <summary>
    /// Rate of training data lost
    /// </summary>
    property TrainLoss: Extended read FTrainLoss write FTrainLoss;
    /// <summary>
    /// Rate of validating data lost
    /// </summary>
    property ValidLoss: Extended read FValidLoss write FValidLoss;
    /// <summary>
    /// Valid Mean Token Accuracy
    /// </summary>
    property ValidMeanTokenAccuracy: Extended read FValidMeanTokenAccuracy write FValidMeanTokenAccuracy;
  end;

  TJobOutCheckpoints = class
  private
    [JsonNameAttribute('metrics')]
    FMetrics: TJobOutMetrics;
    [JsonNameAttribute('step_number')]
    FStepNumber: Int64;
    [JsonNameAttribute('created_at')]
    FCreatedAt: Int64;
  public
    /// <summary>
    /// Metrics at the step number during the fine-tuning job. Use these metrics to assess if the training
    /// is going smoothly (loss should decrease, token accuracy should increase)
    /// </summary>
    property Metrics: TJobOutMetrics read FMetrics write FMetrics;
    /// <summary>
    /// The step number that the checkpoint was created at
    /// </summary>
    property StepNumber: Int64 read FStepNumber write FStepNumber;
    /// <summary>
    /// The UNIX timestamp (in seconds) for when the checkpoint was created
    /// </summary>
    property CreatedAt: Int64 read FCreatedAt write FCreatedAt;

    destructor Destroy; override;
  end;

  TJobOutProgress = class(TJobOut)
  private
    [JsonNameAttribute('events')]
    FEvents: TJobOutEvent;
    [JsonNameAttribute('checkpoints')]
    FCheckpoints: TArray<TJobOutCheckpoints>;
  public
    /// <summary>
    /// Event items are created every time the status of a fine-tuning job changes
    /// The timestamped list of all events is accessible here
    /// </summary>
    /// <remarks>
    /// Default value returned : []
    /// </remarks>
    property Events: TJobOutEvent read FEvents write FEvents;
    /// <summary>
    /// Table with detail of all intermediate points including monitoring of metrics
    /// </summary>
    property Checkpoints: TArray<TJobOutCheckpoints> read FCheckpoints write FCheckpoints;

    destructor Destroy; override;
  end;

  TFineTuningRoute = class(TMistralAIAPIRoute)
    /// <summary>
    /// Get a list of fine tuning jobs for your organization and user
    /// </summary>
    function List(ParamProc: TProc<TFineTuningJobListParams>): TListFineTuningJobs;
    /// <summary>
    /// Create a new fine tuning job, it will be queued for processing.
    /// </summary>
    /// <remarks>
    /// The job is started and the query returns the job ID along with some of the input parameters.
    /// This corresponds to dry_run parameter equal to False
    /// </remarks>
    function CreateAndRun(ParamProc: TProc<TFineTuningJobParams>): TJobOut;
    /// <summary>
    /// Create a new fine tuning job, it will be queued for processing.
    /// </summary>
    /// <remarks>
    /// The job is not spawned, instead the query returns a handful of useful metadata for the user
    /// to perform sanity checks
    /// This corresponds to dry_run parameter equal to True
    /// </remarks>
    function CreateAndPerformSanityCheck(ParamProc: TProc<TFineTuningJobParams>): JobMetadata;
    /// <summary>
    /// Get a fine tuned job details by its UUID
    /// </summary>
    /// <param name="Value">
    /// The ID of the job to analyse
    /// </param>
    function Retrieve(const Value: string): TJobOutProgress;
    /// <summary>
    /// Request the cancellation of a fine tuning job
    /// </summary>
    /// <param name="Value">
    /// The ID of the job to cancel
    /// </param>
    function Cancel(const Value: string): TJobOutProgress;
  end;

implementation

uses
  System.StrUtils, Rest.Json, System.Rtti;

{ TFineTuningJobStatusHelper }

class function TFineTuningJobStatusHelper.Create(
  const Value: string): TFineTuningJobStatus;
begin
  case IndexStr(AnsiLowerCase(Value), [
    'QUEUED', 'STARTED', 'RUNNING', 'FAILED', 'SUCCESS', 'CANCELLED', 'CANCELLATION_REQUESTED']) of
    0 :
      Exit(Queued);
    1 :
      Exit(Started);
    2 :
      Exit(Running);
    3 :
      Exit(Failed);
    4 :
      Exit(Success);
    5 :
      Exit(Cancelled);
    6 :
      Exit(CancellationRequested);
  end;
  Result := Failed;
end;

function TFineTuningJobStatusHelper.ToString: string;
begin
  case Self of
    Queued:
      Exit('QUEUED');
    Started:
      Exit('STARTED');
    Running:
      Exit('RUNNING');
    Failed:
      Exit('FAILED');
    Success:
      Exit('SUCCESS');
    Cancelled:
      Exit('CANCELLED');
    CancellationRequested:
      Exit('CANCELLATION_REQUESTED');
  end;
end;

{ TFineTuningJobListParams }

function TFineTuningJobListParams.CreatedAfter(
  const Value: string): TFineTuningJobListParams;
begin
  Result := TFineTuningJobListParams(Add('created_after', Value));
end;

function TFineTuningJobListParams.CreatedByMe(
  const Value: Boolean): TFineTuningJobListParams;
begin
  Result := TFineTuningJobListParams(Add('created_by_me', Value));
end;

function TFineTuningJobListParams.Model(
  const Value: string): TFineTuningJobListParams;
begin
  Result := TFineTuningJobListParams(Add('model', Value));
end;

function TFineTuningJobListParams.Page(const Value: Int64): TFineTuningJobListParams;
begin
  Result := TFineTuningJobListParams(Add('page', Value));
end;

function TFineTuningJobListParams.PageSize(
  const Value: Int64): TFineTuningJobListParams;
begin
  Result := TFineTuningJobListParams(Add('page_size', Value));
end;

function TFineTuningJobListParams.Status(
  const Value: TFineTuningJobStatus): TFineTuningJobListParams;
begin
  Result := TFineTuningJobListParams(Add('status', Value.ToString));
end;

function TFineTuningJobListParams.Suffix(
  const Value: string): TFineTuningJobListParams;
begin
  Result := TFineTuningJobListParams(Add('suffix', Value));
end;

function TFineTuningJobListParams.WandbName(
  const Value: string): TFineTuningJobListParams;
begin
  Result := TFineTuningJobListParams(Add('wandb_name', Value));
end;

function TFineTuningJobListParams.WandbProject(
  const Value: string): TFineTuningJobListParams;
begin
  Result := TFineTuningJobListParams(Add('wandb_project', Value));
end;

{ TJobOut }

destructor TJobOut.Destroy;
begin
  if Assigned(FHyperparameters) then
    FHyperparameters.Free;
  for var Item in FIntegrations do
    begin
      if Assigned(Item) then
        Item.Free;
    end;
  inherited;
end;

{ TFineTuningJobStatusInterceptor }

function TFineTuningJobStatusInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TFineTuningJobStatus>.ToString;
end;

procedure TFineTuningJobStatusInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TFineTuningJobStatus.Create(Arg)));
end;

{ TFineTuningDataObjectKindHelper }

class function TFineTuningDataObjectKindHelper.Create(
  const Value: string): TFineTuningDataObjectKind;
begin
  case IndexStr(AnsiLowerCase(Value), ['job']) of
    0 :
      Exit(Job);
    else
      raise Exception.CreateFmt('(Fine tuning: TFineTuningDataObjectKind) %s is not an enum value', [Value]);
  end;
end;

function TFineTuningDataObjectKindHelper.ToString: string;
begin
  case Self of
    Job:
      Exit('job');
    else
      raise Exception.Create('(Fine tuning) error converting object to string');
  end;
end;

{ TFineTuningDataObjectKindInterceptor }

function TFineTuningDataObjectKindInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TFineTuningDataObjectKind>.ToString;
end;

procedure TFineTuningDataObjectKindInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TFineTuningDataObjectKind.Create(Arg)));
end;

{ TFineTuningIntegrationTypeHelper }

class function TFineTuningIntegrationTypeHelper.Create(
  const Value: string): TFineTuningIntegrationType;
begin
  case IndexStr(AnsiLowerCase(Value), ['wandb']) of
    0 :
      Exit(Wandb);
    else
      raise Exception.CreateFmt('(Fine tuning: TFineTuningIntegrationType) %s is not an enum value', [Value]);
  end;
end;

function TFineTuningIntegrationTypeHelper.ToString: string;
begin
  case self of
    Wandb:
      Exit('wandb');
    else
      raise Exception.Create('(Fine tuning) error converting object to string');
  end;
end;

{ TFineTuningIntegrationTypeInterceptor }

function TFineTuningIntegrationTypeInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TFineTuningIntegrationType>.ToString;
end;

procedure TFineTuningIntegrationTypeInterceptor.StringReverter(Data: TObject;
  Field, Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TFineTuningIntegrationType.Create(Arg)));
end;

{ TFineTuningObjectKindHelper }

class function TFineTuningObjectKindHelper.Create(
  const Value: string): TFineTuningObjectKind;
begin
  case IndexStr(AnsiLowerCase(Value), ['list']) of
    0 :
      Exit(List);
    else
      raise Exception.CreateFmt('(Fine tuning: FineTuningObjectKind) %s is not an enum value', [Value]);
  end;
end;

function TFineTuningObjectKindHelper.ToString: string;
begin
  case self of
    List:
      Exit('list');
    else
      raise Exception.Create('(Fine tuning) error converting object to string');
  end;
end;

{ TFineTuningObjectKindInterceptor }

function TFineTuningObjectKindInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TFineTuningObjectKind>.ToString;
end;

procedure TFineTuningObjectKindInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TFineTuningObjectKind.Create(Arg)));
end;

{ TFineTuningRoute }

function TFineTuningRoute.Cancel(const Value: string): TJobOutProgress;
begin
  var parameter := Format('fine_tuning/jobs/%s/cancel', [Value]);
  Result := API.Post<TJobOutProgress>(parameter);
end;

function TFineTuningRoute.CreateAndPerformSanityCheck(
  ParamProc: TProc<TFineTuningJobParams>): JobMetadata;
begin
  Result := API.Post<JobMetadata, TFineTuningJobParams>('fine_tuning/jobs', ParamProc);
end;

function TFineTuningRoute.CreateAndRun(
  ParamProc: TProc<TFineTuningJobParams>): TJobOut;
begin
  Result := API.Post<TJobOut, TFineTuningJobParams>('fine_tuning/jobs', ParamProc);
end;

function TFineTuningRoute.List(
  ParamProc: TProc<TFineTuningJobListParams>): TListFineTuningJobs;
begin
  Result := API.Post<TListFineTuningJobs, TFineTuningJobListParams>('fine_tuning/jobs', ParamProc);
end;

function TFineTuningRoute.Retrieve(const Value: string): TJobOutProgress;
begin
  var parameter := Format('fine_tuning/jobs/%s', [Value]);
  Result := API.Get<TJobOutProgress>(parameter);
end;

{ TListFineTuningJobs }

destructor TListFineTuningJobs.Destroy;
begin
  for var Item in Data do
    if Assigned(Item) then
      Item.Free;
  inherited;
end;

{ TFineTuningJobParams }

function TFineTuningJobParams.Hyperparameters(
  Value: THyperparametersParam): TFineTuningJobParams;
var
  JSon: TJSONObject;
begin
  JSon := TJSONObject.Create;

  {--- The TrainingSteps value must be greater or equal to 1 }
  if Value.TrainingSteps < 1 then
    Value.TrainingSteps := 1;
  JSon.AddPair('training_steps', Value.TrainingSteps);

  {--- The LearningRate value must be in the range [1e-8..1] }
  if (Value.LearningRate < 1e-8) or (Value.LearningRate > 1) then
    Value.LearningRate := THyperparametersParam.LearningRateDefault;
  JSon.AddPair('learning_rate', Value.LearningRate);

  Result := TFineTuningJobParams(Add('hyperparameters', JSon));
end;

function TFineTuningJobParams.Integrations(
  const Value: TArray<TJobIntegrationsParam>): TFineTuningJobParams;
var
  Items: TJSONArray;
  JSon: TJSONObject;
begin
  Items := TJSONArray.Create;
  for var Item in Value do
    begin
      JSon := TJSONObject.Create;
      JSon.AddPair('type', Item.&Type.ToString);
      JSon.AddPair('project', Item.Project);
      JSon.AddPair('name', Item.Name);
      JSon.AddPair('api_key', Item.ApiKey);
      Items.Add(JSon);
    end;
  Result := TFineTuningJobParams(Add('integrations', Items));
end;

function TFineTuningJobParams.Model(const Value: string): TFineTuningJobParams;
begin
  Result := TFineTuningJobParams(Add('model', Value));
end;

function TFineTuningJobParams.Suffix(const Value: string): TFineTuningJobParams;
begin
  var S := Trim(Value);
  if Length(S) > 18 then
    raise Exception.CreateFmt('%s : The length of the "suffix" string cannot exceed 18 characters', [S]);
  Result := TFineTuningJobParams(Add('suffix', S));
end;

function TFineTuningJobParams.TrainingFiles(
  const Value: TArray<string>): TFineTuningJobParams;
begin
  Result := TFineTuningJobParams(Add('training_files', Value));
end;

function TFineTuningJobParams.ValidationFiles(
  const Value: TArray<string>): TFineTuningJobParams;
begin
  Result := TFineTuningJobParams(Add('validation_files', Value));
end;

{ THyperparametersParam }

class function THyperparametersParam.Create(const ATrainingSteps: Int64;
  const ALearningRate: Extended): THyperparametersParam;
begin
  Result.TrainingSteps := ATrainingSteps;
  Result.LearningRate := ALearningRate;
end;

procedure THyperparametersParam.SetLearningRate(const Value: Extended);
begin
  if (Value < 1e-8) or (Value > 1) then
    FLearningRate := LearningRateDefault else
    FLearningRate := Value;
end;

procedure THyperparametersParam.SetTrainingSteps(const Value: Int64);
begin
  if Value < 1 then
    FTrainingSteps := 1 else
    FTrainingSteps := Value;
end;

{ TJobIntegrationsParam }

class function TJobIntegrationsParam.Create(const AType: TFineTuningIntegrationType;
  const Project, Name, ApiKey: string): TJobIntegrationsParam;
begin
  Result.&Type := AType;
  Result.Project := Project;
  Result.Name := Name;
  Result.ApiKey := ApiKey;
end;

class function TJobIntegrationsParam.Wandb(const Project, Name,
  ApiKey: string): TJobIntegrationsParam;
begin
  Result := TJobIntegrationsParam.Create(TFineTuningIntegrationType.Wandb, Project, Name, ApiKey);
end;

{ TJobOutCheckpoints }

destructor TJobOutCheckpoints.Destroy;
begin
  if Assigned(FMetrics) then
   FMetrics.Free;
  inherited;
end;

{ TJobOutProgress }

destructor TJobOutProgress.Destroy;
begin
  if Assigned(FEvents) then
    FEvents.Free;
  for var Item in FCheckpoints do
    if Assigned(Item) then
      Item.Free;
  inherited;
end;

end.

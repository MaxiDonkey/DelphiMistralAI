unit MistralAI.FineTunings;

{-------------------------------------------------------------------------------

  **Important Notice**
                                                                                                                                                            |
  To monitor and track the progress of fine-tuning using Wandb.ai tools,
  you are required to register on the Wandb website at   https://wandb.ai/site.

  Registration is necessary to obtain the API key needed for "job" integration
  (TJobIntegrationsParam).

      Github repository : https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

-------------------------------------------------------------------------------}

interface

uses
  System.Classes, System.SysUtils, REST.JsonReflect, System.JSON, REST.Json.Types,
  MistralAI.API.Params, MistralAI.API, MistralAI.Async.Support;

type
  /// <summary>
  /// Enum of the different statuses for a fine-tuning job.
  /// </summary>
  /// <remarks>
  /// Represents the various statuses that a fine-tuning job can have during its lifecycle.
  /// </remarks>
  TFineTuningJobStatus = (
    /// <summary>
    /// The job is queued and waiting to start.
    /// </summary>
    Queued,
    /// <summary>
    /// The job has started
    /// </summary>
    Started,
    /// <summary>
    /// The job is currently running.
    /// </summary>
    Running,
    /// <summary>
    /// The job has failed.
    /// </summary>
    Failed,
    /// <summary>
    /// The job ended successfully.
    /// </summary>
    Success,
    /// <summary>
    /// The job has been cancelled.
    /// </summary>
    Cancelled,
    /// <summary>
    /// A cancellation request has been made; the job is awaiting cancellation.
    /// </summary>
    CancellationRequested);

  /// <summary>
  /// Provides helper methods for the TFineTuningJobStatus enum.
  /// </summary>
  /// <remarks>
  /// Includes methods to convert enum values to strings and create enum values from strings.
  /// </remarks>
  TFineTuningJobStatusHelper = record helper for TFineTuningJobStatus
    /// <summary>
    /// Converts the TFineTuningJobStatus value to its string representation.
    /// </summary>
    /// <returns>
    /// The string representation of the TFineTuningJobStatus value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Creates a TFineTuningJobStatus enum value from a string.
    /// </summary>
    /// <param name="Value">
    /// The string representation of the TFineTuningJobStatus.
    /// </param>
    /// <returns>
    /// The corresponding TFineTuningJobStatus enum value.
    /// </returns>
    class function Create(const Value: string): TFineTuningJobStatus; static;
  end;

  /// <summary>
  /// JSON interceptor for converting TFineTuningJobStatus enum values to strings and vice versa during JSON serialization and deserialization.
  /// </summary>
  TFineTuningJobStatusInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts a TFineTuningJobStatus enum value to its string representation for JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to convert.
    /// </param>
    /// <param name="Field">
    /// The name of the field to convert.
    /// </param>
    /// <returns>
    /// The string representation of the TFineTuningJobStatus enum value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string from JSON deserialization back to a TFineTuningJobStatus enum value.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to revert.
    /// </param>
    /// <param name="Field">
    /// The name of the field to revert.
    /// </param>
    /// <param name="Arg">
    /// The string value to convert back to the enum.
    /// </param>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// Represents the kind of data object in fine-tuning operations.
  /// </summary>
  /// <remarks>
  /// Currently only 'Job' is defined.
  /// </remarks>
  TFineTuningDataObjectKind = (
    /// <summary>
    /// Indicates a fine-tuning job object.
    /// </summary>
    Job
  );

  /// <summary>
  /// Provides helper methods for the TFineTuningDataObjectKind enum.
  /// </summary>
  /// <remarks>
  /// Includes methods to convert enum values to strings and create enum values from strings.
  /// </remarks>
  TFineTuningDataObjectKindHelper = record helper for TFineTuningDataObjectKind
    /// <summary>
    /// Converts the TFineTuningDataObjectKind value to its string representation.
    /// </summary>
    /// <returns>
    /// The string representation of the TFineTuningDataObjectKind value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Creates a TFineTuningDataObjectKind enum value from a string.
    /// </summary>
    /// <param name="Value">
    /// The string representation of the TFineTuningDataObjectKind.
    /// </param>
    /// <returns>
    /// The corresponding TFineTuningDataObjectKind enum value.
    /// </returns>
    class function Create(const Value: string): TFineTuningDataObjectKind; static;
  end;

  /// <summary>
  /// JSON interceptor for converting TFineTuningDataObjectKind enum values to strings and vice versa during JSON serialization and deserialization.
  /// </summary>
  TFineTuningDataObjectKindInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts a TFineTuningDataObjectKind enum value to its string representation for JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to convert.
    /// </param>
    /// <param name="Field">
    /// The name of the field to convert.
    /// </param>
    /// <returns>
    /// The string representation of the TFineTuningDataObjectKind enum value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string from JSON deserialization back to a TFineTuningDataObjectKind enum value.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to revert.
    /// </param>
    /// <param name="Field">
    /// The name of the field to revert.
    /// </param>
    /// <param name="Arg">
    /// The string value to convert back to the enum.
    /// </param>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// Type of platform with which to integrate monitoring information for fine-tuning operations.
  /// </summary>
  TFineTuningIntegrationType = (
    // <summary>
    /// See "Weights and Biases" solutions at the website "https://wandb.ai/site".
    /// </summary>
    Wandb
  );

  /// <summary>
  /// Provides helper methods for the TFineTuningIntegrationType enum.
  /// </summary>
  /// <remarks>
  /// Includes methods to convert enum values to strings and create enum values from strings.
  /// </remarks>
  TFineTuningIntegrationTypeHelper = record helper for TFineTuningIntegrationType
    /// <summary>
    /// Converts the TFineTuningIntegrationType value to its string representation.
    /// </summary>
    /// <returns>
    /// The string representation of the TFineTuningIntegrationType value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Creates a TFineTuningIntegrationType enum value from a string.
    /// </summary>
    /// <param name="Value">
    /// The string representation of the TFineTuningIntegrationType.
    /// </param>
    /// <returns>
    /// The corresponding TFineTuningIntegrationType enum value.
    /// </returns>
    class function Create(const Value: string): TFineTuningIntegrationType; static;
  end;

  /// <summary>
  /// JSON interceptor for converting TFineTuningIntegrationType enum values to strings and vice versa during JSON serialization and deserialization.
  /// </summary>
  TFineTuningIntegrationTypeInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts a TFineTuningIntegrationType enum value to its string representation for JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to convert.
    /// </param>
    /// <param name="Field">
    /// The name of the field to convert.
    /// </param>
    /// <returns>
    /// The string representation of the TFineTuningIntegrationType enum value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string from JSON deserialization back to a TFineTuningIntegrationType enum value.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to revert.
    /// </param>
    /// <param name="Field">
    /// The name of the field to revert.
    /// </param>
    /// <param name="Arg">
    /// The string value to convert back to the enum.
    /// </param>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// Represents the kind of object returned by fine-tuning operations.
  /// </summary>
  /// <remarks>
  /// Currently only 'List' is defined.
  /// </remarks>
  TFineTuningObjectKind = (
    /// <summary>
    /// Indicates a list of fine-tuning jobs.
    /// </summary>
    List
  );

  /// <summary>
  /// Provides helper methods for the TFineTuningObjectKind enum.
  /// </summary>
  /// <remarks>
  /// Includes methods to convert enum values to strings and create enum values from strings.
  /// </remarks>
  TFineTuningObjectKindHelper = record helper for TFineTuningObjectKind
    /// <summary>
    /// Converts the TFineTuningObjectKind value to its string representation.
    /// </summary>
    /// <returns>
    /// The string representation of the TFineTuningObjectKind value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Creates a TFineTuningObjectKind enum value from a string.
    /// </summary>
    /// <param name="Value">
    /// The string representation of the TFineTuningObjectKind.
    /// </param>
    /// <returns>
    /// The corresponding TFineTuningObjectKind enum value.
    /// </returns>
    class function Create(const Value: string): TFineTuningObjectKind; static;
  end;

  /// <summary>
  /// JSON interceptor for converting TFineTuningObjectKind enum values to strings and vice versa during JSON serialization and deserialization.
  /// </summary>
  TFineTuningObjectKindInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts a TFineTuningObjectKind enum value to its string representation for JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to convert.
    /// </param>
    /// <param name="Field">
    /// The name of the field to convert.
    /// </param>
    /// <returns>
    /// The string representation of the TFineTuningObjectKind enum value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string from JSON deserialization back to a TFineTuningObjectKind enum value.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to revert.
    /// </param>
    /// <param name="Field">
    /// The name of the field to revert.
    /// </param>
    /// <param name="Arg">
    /// The string value to convert back to the enum.
    /// </param>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// Represents the parameters for listing fine-tuning jobs.
  /// </summary>
  /// <remarks>
  /// Allows filtering and pagination of fine-tuning jobs when retrieving them via the API.
  /// </remarks>
  TFineTuningJobListParams = class(TJSONParam)
    /// <summary>
    /// Sets the page number of the results to be returned.
    /// </summary>
    /// <param name="Value">
    /// The page number (starting from 0).
    /// </param>
    /// <returns>
    /// The <see cref="TFineTuningJobListParams"/> instance with the updated page number.
    /// </returns>
    /// <remarks> Default: 0 </remarks>
    function Page(const Value: Int64): TFineTuningJobListParams;
    /// <summary>
    /// Sets the number of items to return per page.
    /// </summary>
    /// <param name="Value">
    /// The number of items per page.
    /// </param>
    /// <returns>
    /// The <see cref="TFineTuningJobListParams"/> instance with the updated page size.
    /// </returns>
    /// <remarks> Default: 100 </remarks>
    function PageSize(const Value: Int64): TFineTuningJobListParams;
    /// <summary>
    /// Filters the fine-tuning jobs by model name.
    /// </summary>
    /// <param name="Value">
    /// The model name used for fine-tuning to filter on.
    /// </param>
    /// <returns>
    /// The <see cref="TFineTuningJobListParams"/> instance with the updated model filter.
    /// </returns>
    function Model(const Value: string): TFineTuningJobListParams;
    /// <summary>
    /// Filters the fine-tuning jobs by their current status.
    /// </summary>
    /// <param name="Value">
    /// The current job state to filter on.
    /// </param>
    /// <returns>
    /// The <see cref="TFineTuningJobListParams"/> instance with the updated status filter.
    /// </returns>
    function Status(const Value: TFineTuningJobStatus): TFineTuningJobListParams;
    /// <summary>
    /// Filters the fine-tuning jobs created after a specific date/time.
    /// </summary>
    /// <param name="Value">
    /// The date/time (ISO 8601 format) to filter on.
    /// </param>
    /// <returns>
    /// The <see cref="TFineTuningJobListParams"/> instance with the updated creation date filter.
    /// </returns>
    function CreatedAfter(const Value: string): TFineTuningJobListParams;
    /// <summary>
    /// Filters to only include jobs created by the API caller.
    /// </summary>
    /// <param name="Value">
    /// Set to <c>true</c> to include only jobs created by the API caller.
    /// </param>
    /// <returns>
    /// The <see cref="TFineTuningJobListParams"/> instance with the updated creator filter.
    /// </returns>
    /// <remarks> Default: false </remarks>
    function CreatedByMe(const Value: Boolean = True): TFineTuningJobListParams;
    /// <summary>
    /// Filters the fine-tuning jobs by Weights and Biases project name.
    /// </summary>
    /// <param name="Value">
    /// The WandB project name to filter on.
    /// </param>
    /// <returns>
    /// The <see cref="TFineTuningJobListParams"/> instance with the updated WandB project filter.
    /// </returns>
    function WandbProject(const Value: string): TFineTuningJobListParams;
    /// <summary>
    /// Filters the fine-tuning jobs by Weights and Biases run name.
    /// </summary>
    /// <param name="Value">
    /// The WandB run name to filter on.
    /// </param>
    /// <returns>
    /// The <see cref="TFineTuningJobListParams"/> instance with the updated WandB run name filter.
    /// </returns>
    function WandbName(const Value: string): TFineTuningJobListParams;
    /// <summary>
    /// Filters the fine-tuning jobs by model suffix.
    /// </summary>
    /// <param name="Value">
    /// The model suffix to filter on.
    /// </param>
    /// <returns>
    /// The <see cref="TFineTuningJobListParams"/> instance with the updated suffix filter.
    /// </returns>
    function Suffix(const Value: string): TFineTuningJobListParams;
  end;

  /// <summary>
  /// Represents the hyperparameters used in a fine-tuning job.
  /// </summary>
  /// <remarks>
  /// Includes settings such as the number of training steps and the learning rate.
  /// </remarks>
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
    /// samples from the training dataset.
    /// </summary>
    /// <remarks>
    /// Must be greater than or equal to 1. This property is required.
    /// </remarks>
    property TrainingSteps: Int64 read FTrainingSteps write SetTrainingSteps;
    /// <summary>
    /// The learning rate for the fine-tuning process. It describes how much to adjust the pre-trained model's weights in response to
    /// the estimated error each time the weights are updated during the fine-tuning process.
    /// </summary>
    /// <remarks>
    /// Must be in the range [1e-8..1]. Default value: 0.0001.
    /// </remarks>
    property LearningRate: Extended read FLearningRate write SetLearningRate;
    /// <summary>
    /// Creates a <see cref="THyperparametersParam"/> instance with the specified training steps and learning rate.
    /// </summary>
    /// <param name="ATrainingSteps">
    /// The number of training steps to perform.
    /// </param>
    /// <param name="ALearningRate">
    /// The learning rate for the fine-tuning process.
    /// </param>
    /// <returns>
    /// A <see cref="THyperparametersParam"/> instance initialized with the provided values.
    /// </returns>
    /// <remarks>
    /// Defaults to 1 training step and a learning rate of 0.0001 if not specified.
    /// </remarks>
    class function Create(const ATrainingSteps: Int64 = 1; const ALearningRate: Extended = LearningRateDefault): THyperparametersParam; static;
  end;

  /// <summary>
  /// Represents the integration parameters for a fine-tuning job.
  /// </summary>
  /// <remarks>
  /// Specifies details for integrating with external platforms for monitoring fine-tuning jobs, such as Weights and Biases.
  /// </remarks>
  TJobIntegrationsParam = record
  private
    FType: TFineTuningIntegrationType;
    FProject: string;
    FName: string;
    FApiKey: string;
  public
    /// <summary>
    /// The type of integration.
    /// </summary>
    /// <remarks>
    /// Default: "wandb". Currently, only "wandb" (Weights and Biases) is supported. This property is required.
    /// </remarks>
    property &Type: TFineTuningIntegrationType read FType write FType;
    /// <summary>
    /// The name of the project under which the new run will be created.
    /// </summary>
    /// <remarks> This property is required. </remarks>
    property Project: string read FProject write FProject;
    /// <summary>
    /// A display name to set for the run. If not set, will use the job ID as the name.
    /// </summary>
    property Name: string read FName write FName;
    /// <summary>
    /// The WandB API key to use for authentication.
    /// </summary>
    /// <remarks> This property is required. </remarks>
    property ApiKey: string read FApiKey write FApiKey;
    /// <summary>
    /// Creates a <see cref="TJobIntegrationsParam"/> instance with the specified integration type, project name, run name, and API key.
    /// </summary>
    /// <param name="AType">
    /// The type of integration (e.g., Wandb).
    /// </param>
    /// <param name="Project">
    /// The name of the project under which the run will be created.
    /// </param>
    /// <param name="Name">
    /// A display name for the run.
    /// </param>
    /// <param name="ApiKey">
    /// The API key for authentication with the integration platform.
    /// </param>
    /// <returns>
    /// A <see cref="TJobIntegrationsParam"/> instance initialized with the provided values.
    /// </returns>
    class function Create(const AType: TFineTuningIntegrationType; const Project, Name, ApiKey: string): TJobIntegrationsParam; static;
    /// <summary>
    /// Creates a <see cref="TJobIntegrationsParam"/> instance for a Weights and Biases integration.
    /// </summary>
    /// <param name="Project">
    /// The name of the WandB project under which the run will be created.
    /// </param>
    /// <param name="Name">
    /// A display name for the run.
    /// </param>
    /// <param name="ApiKey">
    /// The WandB API key for authentication.
    /// </param>
    /// <returns>
    /// A <see cref="TJobIntegrationsParam"/> instance initialized for WandB integration.
    /// </returns>
    class function Wandb(const Project, Name, ApiKey: string): TJobIntegrationsParam; static;
  end;

  /// <summary>
  /// Represents the parameters for creating a fine-tuning job.
  /// </summary>
  /// <remarks>
  /// Includes settings such as the model to fine-tune, training files, hyperparameters, and integrations.
  /// </remarks>
  TFineTuningJobParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the name of the model to fine-tune.
    /// </summary>
    /// <param name="Value">
    /// The name of the fine-tunable model.
    /// </param>
    /// <returns>
    /// The <see cref="TFineTuningJobParams"/> instance with the updated model name.
    /// </returns>
    /// <remarks> This property is required. </remarks>
    function Model(const Value: string): TFineTuningJobParams;
    /// <summary>
    /// Sets the IDs of uploaded files that contain training data.
    /// </summary>
    /// <param name="Value">
    /// An array of file IDs for training data.
    /// </param>
    /// <returns>
    /// The <see cref="TFineTuningJobParams"/> instance with the updated training files.
    /// </returns>
    /// <remarks> This property is required. </remarks>
    function TrainingFiles(const Value: TArray<string>): TFineTuningJobParams;
    /// <summary>
    /// Sets the IDs of uploaded files that contain validation data.
    /// </summary>
    /// <param name="Value">
    /// An array of file IDs for validation data.
    /// </param>
    /// <returns>
    /// The <see cref="TFineTuningJobParams"/> instance with the updated validation files.
    /// </returns>
    /// <remarks>
    /// If provided, the data is used to generate validation metrics periodically during fine-tuning.
    /// </remarks>
    function ValidationFiles(const Value: TArray<string>): TFineTuningJobParams;
    /// <summary>
    /// Sets the fine-tuning hyperparameter settings used in a fine-tune job.
    /// </summary>
    /// <param name="Value">
    /// The hyperparameters to use for fine-tuning.
    /// </param>
    /// <returns>
    /// The <see cref="TFineTuningJobParams"/> instance with the updated hyperparameters.
    /// </returns>
    /// <remarks> This property is required. </remarks>
    function Hyperparameters(Value: THyperparametersParam): TFineTuningJobParams; overload;
    /// <summary>
    /// Sets a suffix to be added to the fine-tuned model name.
    /// </summary>
    /// <param name="Value">
    /// A string less than 18 characters to be used as a suffix.
    /// </param>
    /// <returns>
    /// The <see cref="TFineTuningJobParams"/> instance with the updated suffix.
    /// </returns>
    function Suffix(const Value: string): TFineTuningJobParams;
    /// <summary>
    /// Sets the integrations to enable for the fine-tuning job.
    /// </summary>
    /// <param name="Value">
    /// An array of integration parameters.
    /// </param>
    /// <returns>
    /// The <see cref="TFineTuningJobParams"/> instance with the updated integrations.
    /// </returns>
    function Integrations(const Value: TArray<TJobIntegrationsParam>): TFineTuningJobParams;
  end;

  /// <summary>
  /// Represents the hyperparameters output in a fine-tuning job response.
  /// </summary>
  /// <remarks>
  /// Contains the hyperparameter settings used during fine-tuning.
  /// </remarks>
  TJobOutHyperparameters = class
  private
    [JsonNameAttribute('training_steps')]
    FTrainingSteps: Int64;
    [JsonNameAttribute('learning_rate')]
    FLearningRate: Extended;
  public
    /// <summary>
    /// The number of training steps performed.
    /// </summary>
    /// <remarks>
    /// An integer value greater than or equal to 1. This value is required.
    /// </remarks>
    property TrainingSteps: Int64 read FTrainingSteps write FTrainingSteps;
    /// <summary>
    /// The learning rate used during fine-tuning.
    /// </summary>
    /// <remarks>
    /// A value in the range [1e-8 .. 1]. This value is required.
    /// </remarks>
    property LearningRate: Extended read FLearningRate write FLearningRate;
  end;

  /// <summary>
  /// Represents the integrations output in a fine-tuning job response.
  /// </summary>
  /// <remarks>
  /// Contains information about integrations enabled for the fine-tuning job.
  /// </remarks>
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
    /// The type of integration.
    /// </summary>
    /// <remarks>
    /// Default: "wandb". Currently, only "wandb" is defined.
    /// </remarks>
    property &Type: TFineTuningIntegrationType read FType write FType;
    /// <summary>
    /// The name of the project that the run was created under.
    /// </summary>
    /// <remarks> This value is required. </remarks>
    property Project: string read FProject write FProject;
    /// <summary>
    /// The display name set for the run.
    /// </summary>
    property Name: string read FName write FName;
  end;

  /// <summary>
  /// Represents the output of a fine-tuning job.
  /// </summary>
  /// <remarks>
  /// Contains details about the fine-tuning job, including its status, parameters, and results.
  /// </remarks>
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
    /// The ID of the job.
    /// </summary>
    /// <remarks> This value is required. </remarks>
    property Id: string read FId write FId;
    /// <summary>
    /// The hyperparameters used in the fine-tuning job.
    /// </summary>
    /// <remarks> This value is required. </remarks>
    property Hyperparameters: TJobOutHyperparameters read FHyperparameters write FHyperparameters;
    /// <summary>
    /// The name of the model that was fine-tuned.
    /// </summary>
    /// <remarks> This value is required. </remarks>
    property Model: string read FModel write FModel;
    /// <summary>
    /// The current status of the fine-tuning job.
    /// </summary>
    /// <remarks> This value is required. </remarks>
    property Status: TFineTuningJobStatus read FStatus write FStatus;
    /// <summary>
    /// The type of job (e.g., "FT" for fine-tuning).
    /// </summary>
    /// <remarks> This value is required. </remarks>
    property JobType: string read FJobType write FJobType;
    /// <summary>
    /// The UNIX timestamp (in seconds) when the fine-tuning job was created.
    /// </summary>
    /// <remarks> This value is required. </remarks>
    property CreatedAt: Int64 read FCreatedAt write FCreatedAt;
    /// <summary>
    /// The UNIX timestamp (in seconds) when the fine-tuning job was last modified.
    /// </summary>
    /// <remarks> This value is required. </remarks>
    property ModifiedAt: Int64 read FModifiedAt write FModifiedAt;
    /// <summary>
    /// A list of IDs of uploaded files that contain training data.
    /// </summary>
    /// <remarks> This value is required. </remarks>
    property TrainingFiles: TArray<string> read FTrainingFiles write FTrainingFiles;
    //// <summary>
    /// A list of IDs of uploaded files that contain validation data.
    /// </summary>
    property ValidationFiles: TArray<string> read FValidationFiles write FValidationFiles;
    /// <summary>
    /// The kind of data object. Default is "job".
    /// </summary>
    /// <remarks>
    /// Currently only "job" is defined.
    /// </remarks>
    property &Object: TFineTuningDataObjectKind read FObject write FObject;
    /// <summary>
    /// The name of the fine-tuned model that was created. The value will be null if the fine-tuning job is still running.
    /// </summary>
    property FineTuningModel: string read FFineTuningModel write FFineTuningModel;
    /// <summary>
    /// A list of integrations enabled for the fine-tuning job.
    /// </summary>
    property Integrations: TArray<TJobOutIntegrations> read FIntegrations write FIntegrations;
    /// <summary>
    /// Destructor for TJobOut.
    /// </summary>
    /// <remarks>
    /// Frees associated objects.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents a list of fine-tuning jobs.
  /// </summary>
  /// <remarks>
  /// Contains an array of fine-tuning job outputs.
  /// </remarks>
  TListFineTuningJobs = class
  private
    [JsonNameAttribute('data')]
    FData: TArray<TJobOut>;
    [JsonReflectAttribute(ctString, rtString, TFineTuningObjectKindInterceptor)]
    FObject: TFineTuningObjectKind;
  public
    /// <summary>
    /// An array of fine-tuning job outputs.
    /// </summary>
    property Data: TArray<TJobOut> read FData write FData;
    /// <summary>
    /// The kind of object. Default is "list".
    /// </summary>
    /// <remarks>
    /// Currently only "list" is defined.
    /// </remarks>
    property &Object: TFineTuningObjectKind read FObject write FObject;
    /// <summary>
    /// Destructor for TListFineTuningJobs.
    /// </summary>
    /// <remarks>
    /// Frees associated objects.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents metadata for a fine-tuning job.
  /// </summary>
  /// <remarks>
  /// Contains calculated values useful for performing sanity checks before starting a job.
  /// </remarks>
  TJobMetadata = class
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

  /// <summary>
  /// Represents an event in the fine-tuning job lifecycle.
  /// </summary>
  /// <remarks>
  /// Contains information about status changes during the job.
  /// </remarks>
  TJobOutEvent = class
  private
    [JsonNameAttribute('name')]
    FName: string;
    [JsonReflectAttribute(ctString, rtString, TFineTuningJobStatusInterceptor)]
    FData: TFineTuningJobStatus;
    [JsonNameAttribute('created_at')]
    FCreatedAt: int64;
  public
    //// <summary>
    /// The name of the event.
    /// </summary>
    /// <remarks>
    /// This value is required.
    /// </remarks>
    property Name: string read FName write FName;
    /// <summary>
    /// The status of the fine-tuning job at the time of the event.
    /// </summary>
    /// <remarks>
    /// Enum values: "QUEUED", "STARTED", "RUNNING", "FAILED", "SUCCESS", "CANCELLED", "CANCELLATION_REQUESTED".
    /// </remarks>
    property Data: TFineTuningJobStatus read FData write FData;
    /// <summary>
    /// The UNIX timestamp (in seconds) of the event.
    /// </summary>
    /// <remarks>
    /// This value is required.
    /// </remarks>
    property CreatedAt: int64 read FCreatedAt write FCreatedAt;
  end;

  /// <summary>
  /// Represents metrics associated with a checkpoint during fine-tuning.
  /// </summary>
  /// <remarks>
  /// Contains loss and accuracy metrics.
  /// </remarks>
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
    /// Training data loss rate.
    /// </summary>
    property TrainLoss: Extended read FTrainLoss write FTrainLoss;
    /// <summary>
    /// Validation data loss rate.
    /// </summary>
    property ValidLoss: Extended read FValidLoss write FValidLoss;
    /// <summary>
    /// Mean token accuracy on the validation set.
    /// </summary>
    property ValidMeanTokenAccuracy: Extended read FValidMeanTokenAccuracy write FValidMeanTokenAccuracy;
  end;

  /// <summary>
  /// Represents a checkpoint during the fine-tuning job.
  /// </summary>
  /// <remarks>
  /// Contains metrics and timing information at a specific step.
  /// </remarks>
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
    /// Metrics at the step number during the fine-tuning job.
    /// </summary>
    /// <remarks>
    /// Use these metrics to assess if the training is progressing as expected (e.g., loss should decrease).
    /// </remarks>
    property Metrics: TJobOutMetrics read FMetrics write FMetrics;
    /// <summary>
    /// The step number at which the checkpoint was created.
    /// </summary>
    property StepNumber: Int64 read FStepNumber write FStepNumber;
    /// <summary>
    /// The UNIX timestamp (in seconds) when the checkpoint was created.
    /// </summary>
    property CreatedAt: Int64 read FCreatedAt write FCreatedAt;
    /// <summary>
    /// Destructor for TJobOutCheckpoints.
    /// </summary>
    /// <remarks>
    /// Frees associated objects.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents detailed progress information for a fine-tuning job.
  /// </summary>
  /// <remarks>
  /// Extends <see cref="TJobOut"/> with events and checkpoints.
  /// </remarks>
  TJobOutProgress = class(TJobOut)
  private
    [JsonNameAttribute('events')]
    FEvents: TJobOutEvent;
    [JsonNameAttribute('checkpoints')]
    FCheckpoints: TArray<TJobOutCheckpoints>;
  public
    /// <summary>
    /// Event items created every time the status of a fine-tuning job changes.
    /// </summary>
    /// <remarks>
    /// The timestamped list of all events is accessible here. Default value is an empty array.
    /// </remarks>
    property Events: TJobOutEvent read FEvents write FEvents;
    /// <summary>
    /// An array with details of all intermediate checkpoints, including monitoring metrics.
    /// </summary>
    property Checkpoints: TArray<TJobOutCheckpoints> read FCheckpoints write FCheckpoints;
    /// <summary>
    /// Destructor for TJobOutProgress.
    /// </summary>
    /// <remarks>
    /// Frees associated objects.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Asynchronous callback parameters for listing fine-tuning jobs.
  /// </summary>
  TAsyncListFineTuningJobsParams = TAsyncCallBack<TListFineTuningJobs>;

  /// <summary>
  /// Asynchronous callback parameters for fine-tuning job output.
  /// </summary>
  TAsyncTJobOutParams = TAsyncCallBack<TJobOut>;

  /// <summary>
  /// Asynchronous callback parameters for job metadata.
  /// </summary>
  TAsyncJobMetadataParams = TAsyncCallBack<TJobMetadata>;

  /// <summary>
  /// Asynchronous callback parameters for fine-tuning job progress.
  /// </summary>
  TAsyncTJobOutProgressParams = TAsyncCallBack<TJobOutProgress>;

  /// <summary>
  /// Provides methods to interact with fine-tuning jobs in the MistralAI API.
  /// </summary>
  /// <remarks>
  /// Includes synchronous and asynchronous methods for listing, creating, retrieving, and cancelling fine-tuning jobs.
  /// </remarks>
  TFineTuningRoute = class(TMistralAIAPIRoute)
    /// <summary>
    /// Asynchronously retrieves a list of fine-tuning jobs based on the specified parameters.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure that configures the parameters for the fine-tuning job list.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns the asynchronous callback parameters.
    /// </param>
    procedure AsyncList(ParamProc: TProc<TFineTuningJobListParams>;
      const CallBacks: TFunc<TAsyncListFineTuningJobsParams>);
    /// <summary>
    /// Asynchronously creates and runs a new fine-tuning job.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure that configures the parameters for the fine-tuning job.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns the asynchronous callback parameters.
    /// </param>
    procedure AsyncCreateAndRun(ParamProc: TProc<TFineTuningJobParams>;
      const CallBacks: TFunc<TAsyncTJobOutParams>);
    /// <summary>
    /// Asynchronously creates a new fine-tuning job and performs a sanity check without starting the job.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure that configures the parameters for the fine-tuning job.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns the asynchronous callback parameters.
    /// </param>
    procedure AsyncCreateAndPerformSanityCheck(ParamProc: TProc<TFineTuningJobParams>;
      const CallBacks: TFunc<TAsyncJobMetadataParams>);
    /// <summary>
    /// Asynchronously retrieves detailed information about a specific fine-tuning job.
    /// </summary>
    /// <param name="Value">
    /// The ID of the job to retrieve.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns the asynchronous callback parameters.
    /// </param>
    procedure ASyncRetrieve(const Value: string;
      const CallBacks: TFunc<TAsyncTJobOutProgressParams>);
    /// <summary>
    /// Asynchronously requests the cancellation of a fine-tuning job.
    /// </summary>
    /// <param name="Value">
    /// The ID of the job to cancel.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns the asynchronous callback parameters.
    /// </param>
    procedure AsyncCancel(const Value: string;
      const CallBacks: TFunc<TAsyncTJobOutProgressParams>);
    /// <summary>
    /// Retrieves a list of fine-tuning jobs based on the specified parameters.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure that configures the parameters for the fine-tuning job list.
    /// </param>
    /// <returns>
    /// A <see cref="TListFineTuningJobs"/> instance containing the list of jobs.
    /// </returns>
    function List(ParamProc: TProc<TFineTuningJobListParams>): TListFineTuningJobs;
    /// <summary>
    /// Creates and runs a new fine-tuning job.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure that configures the parameters for the fine-tuning job.
    /// </param>
    /// <returns>
    /// A <see cref="TJobOut"/> instance containing details about the created job.
    /// </returns>
    /// <remarks>
    /// The job is started immediately after creation.
    /// </remarks>
    function CreateAndRun(ParamProc: TProc<TFineTuningJobParams>): TJobOut;
    /// <summary>
    /// Creates a new fine-tuning job and performs a sanity check without starting the job.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure that configures the parameters for the fine-tuning job.
    /// </param>
    /// <returns>
    /// A <see cref="TJobMetadata"/> instance containing metadata useful for sanity checks.
    /// </returns>
    /// <remarks>
    /// The job is not started; instead, useful metadata is returned.
    /// </remarks>
    function CreateAndPerformSanityCheck(ParamProc: TProc<TFineTuningJobParams>): TJobMetadata;
    /// <summary>
    /// Retrieves detailed information about a specific fine-tuning job.
    /// </summary>
    /// <param name="Value">
    /// The ID of the job to retrieve.
    /// </param>
    /// <returns>
    /// A <see cref="TJobOutProgress"/> instance containing detailed progress information about the job.
    /// </returns>
    function Retrieve(const Value: string): TJobOutProgress;
    /// <summary>
    /// Requests the cancellation of a fine-tuning job.
    /// </summary>
    /// <param name="Value">
    /// The ID of the job to cancel.
    /// </param>
    /// <returns>
    /// A <see cref="TJobOutProgress"/> instance reflecting the updated status of the job.
    /// </returns>
    function Cancel(const Value: string): TJobOutProgress;
  end;

implementation

uses
  System.StrUtils, Rest.Json, System.Rtti;

{ TFineTuningJobStatusHelper }

class function TFineTuningJobStatusHelper.Create(
  const Value: string): TFineTuningJobStatus;
begin
  case IndexStr(AnsiUpperCase(Value), [
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

procedure TFineTuningRoute.AsyncCancel(const Value: string;
  const CallBacks: TFunc<TAsyncTJobOutProgressParams>);
begin
  with TAsyncCallBackExec<TAsyncTJobOutProgressParams, TJobOutProgress>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TJobOutProgress
      begin
        Result := Cancel(Value);
      end);
  finally
    Free;
  end;
end;

procedure TFineTuningRoute.AsyncCreateAndPerformSanityCheck(
  ParamProc: TProc<TFineTuningJobParams>;
  const CallBacks: TFunc<TAsyncJobMetadataParams>);
begin
  with TAsyncCallBackExec<TAsyncJobMetadataParams, TJobMetadata>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TJobMetadata
      begin
        Result := CreateAndPerformSanityCheck(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TFineTuningRoute.AsyncCreateAndRun(
  ParamProc: TProc<TFineTuningJobParams>;
  const CallBacks: TFunc<TAsyncTJobOutParams>);
begin
  with TAsyncCallBackExec<TAsyncTJobOutParams, TJobOut>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TJobOut
      begin
        Result := CreateAndRun(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TFineTuningRoute.AsyncList(ParamProc: TProc<TFineTuningJobListParams>;
  const CallBacks: TFunc<TAsyncListFineTuningJobsParams>);
begin
  with TAsyncCallBackExec<TAsyncListFineTuningJobsParams, TListFineTuningJobs>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TListFineTuningJobs
      begin
        Result := List(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TFineTuningRoute.ASyncRetrieve(const Value: string;
  const CallBacks: TFunc<TAsyncTJobOutProgressParams>);
begin
  with TAsyncCallBackExec<TAsyncTJobOutProgressParams, TJobOutProgress>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TJobOutProgress
      begin
        Result := Retrieve(Value);
      end);
  finally
    Free;
  end;
end;

function TFineTuningRoute.Cancel(const Value: string): TJobOutProgress;
begin
  var parameter := Format('fine_tuning/jobs/%s/cancel', [Value]);
  Result := API.Post<TJobOutProgress>(parameter);
end;

function TFineTuningRoute.CreateAndPerformSanityCheck(
  ParamProc: TProc<TFineTuningJobParams>): TJobMetadata;
begin
  Result := API.Post<TJobMetadata, TFineTuningJobParams>('fine_tuning/jobs', ParamProc);
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

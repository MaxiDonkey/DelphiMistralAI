unit MistralAI.FineTunings;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

-------------------------------------------------------------------------------}

interface

{$REGION 'Dev note'}
  (*

  **Important Notice**
                                                                                                                                                            |
  To monitor and track the progress of fine-tuning using Wandb.ai tools,
  you are required to register on the Wandb website at   https://wandb.ai/site.

  Registration is necessary to obtain the API key needed for "job" integration
  (TJobIntegrationsParam).

  *)
{$ENDREGION}

uses
  System.Classes, System.SysUtils, REST.JsonReflect, System.JSON, REST.Json.Types,
  MistralAI.API.Params, MistralAI.API, MistralAI.Types, MistralAI.Async.Support,
  MistralAI.Async.Promise;

type
  /// <summary>
  /// Represents the parameters for listing fine-tuning jobs.
  /// </summary>
  /// <remarks>
  /// Allows filtering and pagination of fine-tuning jobs when retrieving them via the API.
  /// </remarks>
  TFineTuningJobListParams = class(TURLParam)
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
  THyperparametersParams = class(TJSONParam)
  public
    /// <summary>
    /// The number of training steps to perform. A training step refers to a single update of the model
    /// weights during the fine-tuning process. This update is typically calculated using a batch of
    /// samples from the training dataset.
    /// </summary>
    /// <remarks>
    /// Must be greater than or equal to 1. This property is required.
    /// </remarks>
    function TrainingSteps(const Value: Integer): THyperparametersParams;

    /// <summary>
    /// The learning rate for the fine-tuning process. It describes how much to adjust the pre-trained model's weights in response to
    /// the estimated error each time the weights are updated during the fine-tuning process.
    /// </summary>
    /// <remarks>
    /// Must be in the range [1e-8..1]. Default value: 0.0001.
    /// </remarks>
    function LearningRate(const Value: Double): THyperparametersParams;

    /// <summary>
    /// (Advanced Usage) Weight decay adds a term to the loss function that is proportional to the sum
    /// of the squared weights. This term reduces the magnitude of the weights and prevents them from
    /// growing too large.
    /// </summary>
    function WeightDecay(const Value: Double): THyperparametersParams;

    /// <summary>
    /// (Advanced Usage) A parameter that specifies the percentage of the total training steps at which
    /// the learning rate warm-up phase ends. During this phase, the learning rate gradually increases
    /// from a small value to the initial learning rate, helping to stabilize the training process and
    /// improve convergence. Similar to pct_start in mistral-finetune.
    /// </summary>
    function WarmupFraction(const Value: Double): THyperparametersParams;

    /// <summary>
    /// Set epochs value
    /// </summary>
    function Epochs(const Value: Double): THyperparametersParams;

    /// <summary>
    /// Set fim_ration value
    /// </summary>
    function FimRatio(const Value: Double): THyperparametersParams;

    /// <summary>
    /// Set seq_len value
    /// </summary>
    function SeqLen(const Value: Integer): THyperparametersParams;

    class function New(const ParamProc: TProcRef<THyperparametersParams>): THyperparametersParams;
  end;

  /// <summary>
  /// Represents the integration parameters for a fine-tuning job.
  /// </summary>
  /// <remarks>
  /// Specifies details for integrating with external platforms for monitoring fine-tuning jobs, such as Weights and Biases.
  /// </remarks>
  TJobIntegrationsParams = class(TJSONParam)
  public
    /// <summary>
    /// The type of integration.
    /// </summary>
    /// <remarks>
    /// Default: "wandb". Currently, only "wandb" (Weights and Biases) is supported. This property is required.
    /// </remarks>
    function &Type(const Value: TFineTuningIntegrationType): TJobIntegrationsParams;

    /// <summary>
    /// The name of the project under which the new run will be created.
    /// </summary>
    /// <remarks> This property is required. </remarks>
    function Project(const Value: string): TJobIntegrationsParams;

    /// <summary>
    /// A display name to set for the run. If not set, will use the job ID as the name.
    /// </summary>
    function Name(const Value: string): TJobIntegrationsParams;

    /// <summary>
    /// The WandB API key to use for authentication.
    /// </summary>
    /// <remarks> This property is required. </remarks>
    function ApiKey(const Value: string): TJobIntegrationsParams;

    /// <summary>
    /// A display run name to set for this run.
    /// </summary>
    function RunName(const Value: string): TJobIntegrationsParams;

    class function New(const ParamProc: TProcRef<TJobIntegrationsParams>): TJobIntegrationsParams;
  end;

  /// <summary>
  /// Represents the parameters required for configuring a repository in fine-tuning jobs.
  /// </summary>
  /// <remarks>
  /// This class is used to specify details about a repository, including its type, name, owner, reference,
  /// weight, and token. These parameters are typically used when setting up repositories for fine-tuning jobs.
  /// </remarks>
  TRepositoryParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the type of the repository.
    /// </summary>
    /// <param name="Value">
    /// The type of the repository as an enum value of <see cref="TRepositoryType"/>.
    /// </param>
    /// <returns>
    /// The <see cref="TRepositoryParams"/> instance with the updated repository type.
    /// </returns>
    function &Type(const Value: TRepositoryType): TRepositoryParams;

    /// <summary>
    /// Sets the name of the repository.
    /// </summary>
    /// <param name="Value">
    /// The name of the repository as a string.
    /// </param>
    /// <returns>
    /// The <see cref="TRepositoryParams"/> instance with the updated repository name.
    /// </returns>
    function Name(const Value: string): TRepositoryParams;

    /// <summary>
    /// Sets the owner of the repository.
    /// </summary>
    /// <param name="Value">
    /// The owner of the repository as a string.
    /// </param>
    /// <returns>
    /// The <see cref="TRepositoryParams"/> instance with the updated owner.
    /// </returns>
    function Owner(const Value: string): TRepositoryParams;

    /// <summary>
    /// Sets the reference for the repository.
    /// </summary>
    /// <param name="Value">
    /// The reference for the repository, such as a branch or commit hash, as a string.
    /// </param>
    /// <returns>
    /// The <see cref="TRepositoryParams"/> instance with the updated reference.
    /// </returns>
    function Ref(const Value: string): TRepositoryParams;

    /// <summary>
    /// Sets the weight for the repository.
    /// </summary>
    /// <param name="Value">
    /// The weight assigned to the repository as a double.
    /// </param>
    /// <returns>
    /// The <see cref="TRepositoryParams"/> instance with the updated weight.
    /// </returns>
    function Weight(const Value: Double): TRepositoryParams;

    /// <summary>
    /// Sets the token for accessing the repository.
    /// </summary>
    /// <param name="Value">
    /// The token for authentication as a string.
    /// </param>
    /// <returns>
    /// The <see cref="TRepositoryParams"/> instance with the updated token.
    /// </returns>
    function Token(const Value: string): TRepositoryParams;

    /// <summary>
    /// Creates a new instance of <see cref="TRepositoryParams"/> with configured parameters.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the parameters for the repository.
    /// </param>
    /// <returns>
    /// A new instance of <see cref="TRepositoryParams"/> with the specified parameters.
    /// </returns>
    class function New(const ParamProc: TProcRef<TRepositoryParams>): TRepositoryParams;
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
    function Hyperparameters(Value: THyperparametersParams): TFineTuningJobParams; overload;

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
    function Hyperparameters(ParamProc: TProcRef<THyperparametersParams>): TFineTuningJobParams; overload;

    /// <summary>
    /// Sets a suffix to be added to the fine-tuned model name.
    /// <para>
    /// A string that will be added to your fine-tuning model name. For example, a suffix of
    /// "my-great-model" would produce a model name like ft:open-mistral-7b:my-great-model:xxx...
    /// </para>
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
    function Integrations(const Value: TArray<TJobIntegrationsParams>): TFineTuningJobParams; overload;

    /// <summary>
    /// Set the repositories reference .
    /// </summary>
    /// <param name="Value">
    /// An array of repository.
    /// </param>
    /// <returns>
    /// The <see cref="TFineTuningJobParams"/> instance with the updated integrations.
    /// </returns>
    function Repositories(const Value: TArray<TRepositoryParams>): TFineTuningJobParams;

    /// <summary>
    /// This field will be required in a future release.
    /// </summary>
    /// <param name="Value">
    /// Boolean value to manage the auto start
    /// </param>
    /// <returns>
    /// The <see cref="TFineTuningJobParams"/> instance with the updated integrations.
    /// </returns>
    function AutoStart(const Value: Boolean): TFineTuningJobParams;
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
    FLearningRate: Double;
    [JsonNameAttribute('weight_decay')]
    FWeightDecay: Double;
    [JsonNameAttribute('warmup_fraction')]
    FWarmupFraction: Double;
    FEpochs: Double;
    [JsonNameAttribute('seq_len')]
    FSeqLen: Integer;
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
    property LearningRate: Double read FLearningRate write FLearningRate;

    property WeightDecay: Double read FWeightDecay write FWeightDecay;

    property WarmupFraction: Double read FWarmupFraction write FWarmupFraction;

    property Epochs: Double read FEpochs write FEpochs;

    property SeqLen: Integer read FSeqLen write FSeqLen;
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
    FProject: string;
    FName: string;
    [JsonNameAttribute('run_name')]
    FRunName: string;
    FUrl: string;
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

    property RunName: string read FRunName write FRunName;

    property Url: string read FUrl write FUrl;
  end;

  /// <summary>
  /// Represents a repository used in the context of fine-tuning jobs.
  /// </summary>
  /// <remarks>
  /// This class encapsulates information about a repository, including its type, name, owner, reference, weight, and commit ID.
  /// It is used to provide detailed metadata about the repository in fine-tuning operations.
  /// </remarks>
  TRepository = class
  private
    [JsonReflectAttribute(ctString, rtString, TRepositoryTypeInterceptor)]
    FType: TRepositoryType;
    FName: string;
    FOwner: string;
    FRef: string;
    FWeight: Double;
    [JsonNameAttribute('commit_id')]
    FCommitId: string;
  public
    /// <summary>
    /// Gets or sets the type of the repository.
    /// </summary>
    /// <remarks>
    /// This property is required and determines the type of repository being used, such as "git" or "svn."
    /// </remarks>
    property &Type: TRepositoryType read FType write FType;

    /// <summary>
    /// Gets or sets the name of the repository.
    /// </summary>
    /// <remarks>
    /// This property represents the name of the repository as a string.
    /// </remarks>
    property Name: string read FName write FName;

    /// <summary>
    /// Gets or sets the owner of the repository.
    /// </summary>
    /// <remarks>
    /// This property specifies the owner of the repository, such as the username or organization name.
    /// </remarks>
    property Owner: string read FOwner write FOwner;

    /// <summary>
    /// Gets or sets the reference of the repository.
    /// </summary>
    /// <remarks>
    /// The reference can be a branch name, a tag, or a specific commit hash.
    /// </remarks>
    property Ref: string read FRef write FRef;

    /// <summary>
    /// Gets or sets the weight of the repository.
    /// </summary>
    /// <remarks>
    /// The weight is used to indicate the repository's relative importance or contribution in fine-tuning operations.
    /// </remarks>
    property Weight: Double read FWeight write FWeight;

    /// <summary>
    /// Gets or sets the commit ID of the repository.
    /// </summary>
    /// <remarks>
    /// This property specifies the exact commit used in the operation, ensuring reproducibility.
    /// </remarks>
    property CommitId: string read FCommitId write FCommitId;
  end;

  /// <summary>
  /// Represents metadata associated with data used in fine-tuning jobs.
  /// </summary>
  /// <remarks>
  /// This class provides detailed information about the cost, duration, and token usage
  /// for fine-tuning operations, offering insights into resource consumption.
  /// </remarks>
  TDataMetadata = class
  private
    [JsonNameAttribute('expected_duration_seconds')]
    FExpectedDurationSeconds: Int64;
    FCost: Double;
    [JsonNameAttribute('cost_currency')]
    FCostCurrency: string;
    [JsonNameAttribute('train_tokens_per_step')]
    FTrainTokensPerStep: Int64;
    [JsonNameAttribute('train_tokens')]
    FTrainTokens: Int64;
    [JsonNameAttribute('data_tokens')]
    FDataTokens: Int64;
    [JsonNameAttribute('estimated_start_time')]
    FEstimatedStartTime: Int64;
  public
    /// <summary>
    /// Gets or sets the expected duration of the fine-tuning job, in seconds.
    /// </summary>
    /// <remarks>
    /// This property provides an estimate of how long the job will take to complete.
    /// </remarks>
    property ExpectedDurationSeconds: Int64 read FExpectedDurationSeconds write FExpectedDurationSeconds;

    /// <summary>
    /// Gets or sets the total cost of the fine-tuning job.
    /// </summary>
    /// <remarks>
    /// This property reflects the overall monetary cost of the job.
    /// </remarks>
    property Cost: Double read FCost write FCost;

    /// <summary>
    /// Gets or sets the currency in which the cost is measured.
    /// </summary>
    /// <remarks>
    /// Examples include "USD" or "EUR."
    /// </remarks>
    property CostCurrency: string read FCostCurrency write FCostCurrency;

    /// <summary>
    /// Gets or sets the number of training tokens processed per step during fine-tuning.
    /// </summary>
    /// <remarks>
    /// This value helps analyze the efficiency of token processing in each training step.
    /// </remarks>
    property TrainTokensPerStep: Int64 read FTrainTokensPerStep write FTrainTokensPerStep;

    /// <summary>
    /// Gets or sets the total number of training tokens used in the fine-tuning process.
    /// </summary>
    /// <remarks>
    /// This property provides the cumulative count of tokens used for training the model.
    /// </remarks>
    property TrainTokens: Int64 read FTrainTokens write FTrainTokens;

    /// <summary>
    /// Gets or sets the total number of data tokens processed in the fine-tuning job.
    /// </summary>
    /// <remarks>
    /// This includes all tokens used in the training and validation datasets.
    /// </remarks>
    property DataTokens: Int64 read FDataTokens write FDataTokens;

    /// <summary>
    /// Gets or sets the estimated start time of the fine-tuning job, as a UNIX timestamp.
    /// </summary>
    /// <remarks>
    /// This property provides the predicted start time of the job for scheduling purposes.
    /// </remarks>
    property EstimatedStartTime: Int64 read FEstimatedStartTime write FEstimatedStartTime;
  end;

  TClassifierTarget = class
  private
    FName: string;
    FLabels: TArray<string>;
    FWeight: Double;
    [JsonNameAttribute('loss_function')]
    FLossFunction: string;
  public
    property Name: string read FName write FName;
    property Labels: TArray<string> read FLabels write FLabels;
    property Weight: Double read FWeight write FWeight;
    property LossFunction: string read FLossFunction write FLossFunction;
  end;

  /// <summary>
  /// Represents the output of a fine-tuning job.
  /// </summary>
  /// <remarks>
  /// Contains details about the fine-tuning job, including its status, parameters, and results.
  /// </remarks>
  TJobOut = class(TJSONFingerprint)
  private
    FId: string;
    [JsonNameAttribute('auto_start')]
    FAutoStart: Boolean;
    FHyperparameters: TJobOutHyperparameters;
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
    FSuffix: string;
    FIntegrations: TArray<TJobOutIntegrations>;
    [JsonNameAttribute('trained_tokens')]
    FTrainedTokens: Int64;
    FRepositories: TArray<TRepository>;
    FMetadata: TDataMetadata;
    [JsonNameAttribute('classifier_targets')]
    FClassifierTargets: TArray<TClassifierTarget>;
  public
    /// <summary>
    /// The ID of the job.
    /// </summary>
    /// <remarks> This value is required. </remarks>
    property Id: string read FId write FId;

    /// <summary>
    /// The auto_start of the job.
    /// </summary>
    /// <remarks> This value is required. </remarks>
    property AutoStart: Boolean read FAutoStart write FAutoStart;

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
    /// Optional text/code that adds more context for the model. When given a prompt and a suffix
    /// the model will fill what is between them. When suffix is not provided, the model will simply
    /// execute completion starting with prompt.
    /// </summary>
    property Suffix: string read FSuffix write FSuffix;

    /// <summary>
    /// A list of integrations enabled for the fine-tuning job.
    /// </summary>
    property Integrations: TArray<TJobOutIntegrations> read FIntegrations write FIntegrations;

    /// <summary>
    /// Total number of tokens trained.
    /// </summary>
    property TrainedTokens: Int64 read FTrainedTokens write FTrainedTokens;

    /// <summary>
    /// Array of repositories (e.g. repository on GitHub) max <= 20 items
    /// </summary>
    property Repositories: TArray<TRepository> read FRepositories write FRepositories;

    /// <summary>
    /// JobMetadataOut
    /// </summary>
    property Metadata: TDataMetadata read FMetadata write FMetadata;

    property ClassifierTargets: TArray<TClassifierTarget> read FClassifierTargets write FClassifierTargets;

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
  TListFineTuningJobs = class(TJSONFingerprint)
  private
    FData: TArray<TJobOut>;
    [JsonReflectAttribute(ctString, rtString, TFineTuningObjectKindInterceptor)]
    FObject: TFineTuningObjectKind;
    FTotal: Int64;
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
    /// Count of Data
    /// </summary>
    property Total: Int64 read FTotal write FTotal;

    /// <summary>
    /// Destructor for TListFineTuningJobs.
    /// </summary>
    /// <remarks>
    /// Frees associated objects.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Status and error dot a fine-tune job.
  /// </summary>
  TJobOutEventData = class
  private
    [JsonReflectAttribute(ctString, rtString, TFineTuningJobStatusInterceptor)]
    FStatus: TFineTuningJobStatus;
    FError: string;
  public
    /// <summary>
    /// Status for a fine-tuning job
    /// </summary>
    property Status: TFineTuningJobStatus read FStatus write FStatus;

    /// <summary>
    /// Error message
    /// </summary>
    property Error: string read FError write FError;
  end;

  /// <summary>
  /// Represents an event in the fine-tuning job lifecycle.
  /// </summary>
  /// <remarks>
  /// Contains information about status changes during the job.
  /// </remarks>
  TJobOutEvent = class
  private
    FName: string;
    FData: TJobOutEventData;
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
    property Data: TJobOutEventData read FData write FData;

    /// <summary>
    /// The UNIX timestamp (in seconds) of the event.
    /// </summary>
    /// <remarks>
    /// This value is required.
    /// </remarks>
    property CreatedAt: int64 read FCreatedAt write FCreatedAt;

    destructor Destroy; override;
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
    FTrainLoss: Double;
    [JsonNameAttribute('valid_loss')]
    FValidLoss: Double;
    [JsonNameAttribute('valid_mean_token_accuracy')]
    FValidMeanTokenAccuracy: Double;
  public
    /// <summary>
    /// Training data loss rate.
    /// </summary>
    property TrainLoss: Double read FTrainLoss write FTrainLoss;

    /// <summary>
    /// Validation data loss rate.
    /// </summary>
    property ValidLoss: Double read FValidLoss write FValidLoss;

    /// <summary>
    /// Mean token accuracy on the validation set.
    /// </summary>
    property ValidMeanTokenAccuracy: Double read FValidMeanTokenAccuracy write FValidMeanTokenAccuracy;
  end;

  /// <summary>
  /// Represents a checkpoint during the fine-tuning job.
  /// </summary>
  /// <remarks>
  /// Contains metrics and timing information at a specific step.
  /// </remarks>
  TJobOutCheckpoints = class
  private
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
    FEvents: TArray<TJobOutEvent>;
    FCheckpoints: TArray<TJobOutCheckpoints>;
  public
    /// <summary>
    /// Event items created every time the status of a fine-tuning job changes.
    /// </summary>
    /// <remarks>
    /// The timestamped list of all events is accessible here. Default value is an empty array.
    /// </remarks>
    property Events: TArray<TJobOutEvent> read FEvents write FEvents;

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
  TAsynListFineTuningJobs = TAsyncCallBack<TListFineTuningJobs>;

  /// <summary>
  /// Represents a promise‑based asynchronous callback that resolves to a list of fine‑tuning jobs.
  /// </summary>
  /// <remarks>
  /// This is an alias for <c>TPromiseCallback&lt;TListFineTuningJobs&gt;</c>, allowing callers to await
  /// the result of the <see cref="TFineTuningRoute.List"/> operation in a promise‑style workflow.
  /// </remarks>
  TPromiseListFineTuningJobs = TPromiseCallback<TListFineTuningJobs>;

  /// <summary>
  /// Asynchronous callback parameters for fine-tuning job output.
  /// </summary>
  TAsynJobOut = TAsyncCallBack<TJobOut>;

  /// <summary>
  /// Represents a promise‑based asynchronous callback that resolves to a fine‑tuning job output.
  /// </summary>
  /// <remarks>
  /// This is an alias for <c>TPromiseCallback&lt;TJobOut&gt;</c>, enabling callers to await
  /// the result of the <see cref="TFineTuningRoute.CreateJob"/> operation in a promise‑style workflow.
  /// </remarks>
  TPromiseJobOut = TPromiseCallback<TJobOut>;

  /// <summary>
  /// Asynchronous callback parameters for fine-tuning job progress.
  /// </summary>
  TAsynJobOutProgress = TAsyncCallBack<TJobOutProgress>;

  /// <summary>
  /// Represents a promise‑based asynchronous callback that resolves to detailed fine‑tuning job progress.
  /// </summary>
  /// <remarks>
  /// This is an alias for <c>TPromiseCallback&lt;TJobOutProgress&gt;</c>, allowing callers to await
  /// the result of the <see cref="TFineTuningRoute.Retrieve"/> (or <see cref="TFineTuningRoute.Cancel"/>)
  /// operation in a promise‑style workflow.
  /// </remarks>
  TPromiseJobOutProgress = TPromiseCallback<TJobOutProgress>;

  /// <summary>
  /// Provides methods to interact with fine-tuning jobs in the MistralAI API.
  /// </summary>
  /// <remarks>
  /// Includes synchronous and asynchronous methods for listing, creating, retrieving, and cancelling fine-tuning jobs.
  /// </remarks>
  TFineTuningRoute = class(TMistralAIAPIRoute)
    /// <summary>
    /// Asynchronously retrieves a paginated list of fine‑tuning jobs.
    /// </summary>
    /// <param name="ParamProc">
    /// A callback to configure the <see cref="TFineTuningJobListParams"/> (e.g. page, page size, filters).
    /// </param>
    /// <param name="Callbacks">
    /// An optional function that returns a promise‑style callback wrapper (<see cref="TPromiseListFineTuningJobs"/>)
    /// for handling success, error, and cancellation events.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{TListFineTuningJobs}"/> that resolves to a <see cref="TListFineTuningJobs"/> instance
    /// containing the retrieved jobs when the API call succeeds.
    /// </returns>
    function AsyncAwaitList(ParamProc: TProc<TFineTuningJobListParams>;
      const Callbacks: TFunc<TPromiseListFineTuningJobs> = nil): TPromise<TListFineTuningJobs>;

    /// <summary>
    /// Asynchronously creates and starts a new fine‑tuning job.
    /// </summary>
    /// <param name="ParamProc">
    /// A callback to configure the <see cref="TFineTuningJobParams"/>,
    /// specifying the model, training files, hyperparameters, and other settings.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function that returns a promise‑style callback wrapper (<see cref="TPromiseJobOut"/>)
    /// for handling success, error, and cancellation events.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{TJobOut}"/> that resolves to a <see cref="TJobOut"/> instance
    /// containing details of the created job when the API call succeeds.
    /// </returns>
    function AsyncAwaitCreateJob(ParamProc: TProc<TFineTuningJobParams>;
      const Callbacks: TFunc<TPromiseJobOut> = nil): TPromise<TJobOut>;

    /// <summary>
    /// Asynchronously retrieves detailed progress for an existing fine‑tuning job.
    /// </summary>
    /// <param name="JobId">
    /// The identifier of the fine‑tuning job to retrieve.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function that returns a promise‑style callback wrapper (<see cref="TPromiseJobOutProgress"/>)
    /// for handling success, error, and cancellation events.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{TJobOutProgress}"/> that resolves to a <see cref="TJobOutProgress"/> instance
    /// containing the job’s current status, events, and checkpoints when the API call succeeds.
    /// </returns>
    function AsyncAwaitRetrieve(const JobId: string;
      const Callbacks: TFunc<TPromiseJobOutProgress> = nil): TPromise<TJobOutProgress>;

    /// <summary>
    /// Asynchronously requests cancellation of an active fine‑tuning job.
    /// </summary>
    /// <param name="JobId">
    /// The identifier of the fine‑tuning job to cancel.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function that returns a promise‑style callback wrapper (<see cref="TPromiseJobOutProgress"/>)
    /// for handling success, error, and cancellation confirmation events.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{TJobOutProgress}"/> that resolves to a <see cref="TJobOutProgress"/> instance
    /// reflecting the job’s updated status and progress events when the cancellation request completes.
    /// </returns>
    function AsyncAwaitCancel(const JobId: string;
      const Callbacks: TFunc<TPromiseJobOutProgress> = nil): TPromise<TJobOutProgress>;

    /// <summary>
    /// Asynchronously starts a preconfigured fine‑tuning job.
    /// </summary>
    /// <param name="JobId">
    /// The identifier of the fine‑tuning job to start.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function that returns a promise‑style callback wrapper (<see cref="TPromiseJobOutProgress"/>)
    /// for handling success, error, and cancellation events.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{TJobOutProgress}"/> that resolves to a <see cref="TJobOutProgress"/> instance
    /// containing the job’s updated status, events, and checkpoints when the start request completes.
    /// </returns>
    function AsyncAwaitStart(const JobId: string;
      const Callbacks: TFunc<TPromiseJobOutProgress> = nil): TPromise<TJobOutProgress>;

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
      const CallBacks: TFunc<TAsynListFineTuningJobs>);

    /// <summary>
    /// Asynchronously creates and runs a new fine-tuning job.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure that configures the parameters for the fine-tuning job.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns the asynchronous callback parameters.
    /// </param>
    procedure AsyncCreateJob(ParamProc: TProc<TFineTuningJobParams>;
      const CallBacks: TFunc<TAsynJobOut>);

    /// <summary>
    /// Asynchronously retrieves detailed information about a specific fine-tuning job.
    /// </summary>
    /// <param name="Value">
    /// The ID of the job to retrieve.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns the asynchronous callback parameters.
    /// </param>
    procedure ASyncRetrieve(const JobId: string;
      const CallBacks: TFunc<TAsynJobOutProgress>);

    /// <summary>
    /// Asynchronously requests the cancellation of a fine-tuning job.
    /// </summary>
    /// <param name="Value">
    /// The ID of the job to cancel.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns the asynchronous callback parameters.
    /// </param>
    procedure AsyncCancel(const JobId: string;
      const CallBacks: TFunc<TAsynJobOutProgress>);

    /// <summary>
    /// Asynchronously start a fine-tuning job.
    /// </summary>
    /// <param name="Value">
    /// The ID of the job to cancel.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns the asynchronous callback parameters.
    /// </param>
    procedure AsyncStart(const JobId: string;
      const CallBacks: TFunc<TAsynJobOutProgress>);

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
    function CreateJob(ParamProc: TProc<TFineTuningJobParams>): TJobOut;

    /// <summary>
    /// Retrieves detailed information about a specific fine-tuning job.
    /// </summary>
    /// <param name="Value">
    /// The ID of the job to retrieve.
    /// </param>
    /// <returns>
    /// A <see cref="TJobOutProgress"/> instance containing detailed progress information about the job.
    /// </returns>
    function Retrieve(const JobId: string): TJobOutProgress;

    /// <summary>
    /// Requests the cancellation of a fine-tuning job.
    /// </summary>
    /// <param name="Value">
    /// The ID of the job to cancel.
    /// </param>
    /// <returns>
    /// A <see cref="TJobOutProgress"/> instance reflecting the updated status of the job.
    /// </returns>
    function Cancel(const JobId: string): TJobOutProgress;

    /// <summary>
    /// Start a fine-tuning job.
    /// </summary>
    /// <param name="Value">
    /// The ID of the job to cancel.
    /// </param>
    /// <returns>
    /// A <see cref="TJobOutProgress"/> instance reflecting the updated status of the job.
    /// </returns>
    function Start(const JobId: string): TJobOutProgress;
  end;

implementation

uses
  System.StrUtils, Rest.Json, System.Rtti;

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
    Item.Free;
  for var Item in FRepositories do
    Item.Free;
  if Assigned(FMetadata) then
    FMetadata.Free;
  for var Item in FClassifierTargets do
    Item.Free;
  inherited;
end;

{ TFineTuningRoute }

function TFineTuningRoute.AsyncAwaitCancel(const JobId: string;
  const Callbacks: TFunc<TPromiseJobOutProgress>): TPromise<TJobOutProgress>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TJobOutProgress>(
    procedure(const CallbackParams: TFunc<TAsynJobOutProgress>)
    begin
      AsyncCancel(JobId, CallbackParams);
    end,
    Callbacks);
end;

function TFineTuningRoute.AsyncAwaitCreateJob(
  ParamProc: TProc<TFineTuningJobParams>;
  const Callbacks: TFunc<TPromiseJobOut>): TPromise<TJobOut>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TJobOut>(
    procedure(const CallbackParams: TFunc<TAsynJobOut>)
    begin
      AsyncCreateJob(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TFineTuningRoute.AsyncAwaitList(
  ParamProc: TProc<TFineTuningJobListParams>;
  const Callbacks: TFunc<TPromiseListFineTuningJobs>): TPromise<TListFineTuningJobs>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TListFineTuningJobs>(
    procedure(const CallbackParams: TFunc<TAsynListFineTuningJobs>)
    begin
      AsyncList(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TFineTuningRoute.AsyncAwaitRetrieve(const JobId: string;
  const Callbacks: TFunc<TPromiseJobOutProgress>): TPromise<TJobOutProgress>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TJobOutProgress>(
    procedure(const CallbackParams: TFunc<TAsynJobOutProgress>)
    begin
      AsyncRetrieve(JobId, CallbackParams);
    end,
    Callbacks);
end;

function TFineTuningRoute.AsyncAwaitStart(const JobId: string;
  const Callbacks: TFunc<TPromiseJobOutProgress>): TPromise<TJobOutProgress>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TJobOutProgress>(
    procedure(const CallbackParams: TFunc<TAsynJobOutProgress>)
    begin
      AsyncStart(JobId, CallbackParams);
    end,
    Callbacks);
end;

procedure TFineTuningRoute.AsyncCancel(const JobId: string;
  const CallBacks: TFunc<TAsynJobOutProgress>);
begin
  with TAsyncCallBackExec<TAsynJobOutProgress, TJobOutProgress>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TJobOutProgress
      begin
        Result := Cancel(JobId);
      end);
  finally
    Free;
  end;
end;

procedure TFineTuningRoute.AsyncCreateJob(
  ParamProc: TProc<TFineTuningJobParams>;
  const CallBacks: TFunc<TAsynJobOut>);
begin
  with TAsyncCallBackExec<TAsynJobOut, TJobOut>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TJobOut
      begin
        Result := Self.CreateJob(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TFineTuningRoute.AsyncList(ParamProc: TProc<TFineTuningJobListParams>;
  const CallBacks: TFunc<TAsynListFineTuningJobs>);
begin
  with TAsyncCallBackExec<TAsynListFineTuningJobs, TListFineTuningJobs>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TListFineTuningJobs
      begin
        Result := Self.List(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TFineTuningRoute.ASyncRetrieve(const JobId: string;
  const CallBacks: TFunc<TAsynJobOutProgress>);
begin
  with TAsyncCallBackExec<TAsynJobOutProgress, TJobOutProgress>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TJobOutProgress
      begin
        Result := Self.Retrieve(JobId);
      end);
  finally
    Free;
  end;
end;

procedure TFineTuningRoute.AsyncStart(const JobId: string;
  const CallBacks: TFunc<TAsynJobOutProgress>);
begin
  with TAsyncCallBackExec<TAsynJobOutProgress, TJobOutProgress>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TJobOutProgress
      begin
        Result := Self.Start(JobId);
      end);
  finally
    Free;
  end;
end;

function TFineTuningRoute.Cancel(const JobId: string): TJobOutProgress;
begin
  Result := API.PostEx<TJobOutProgress>(Format('fine_tuning/jobs/%s/cancel', [JobId]));
end;

function TFineTuningRoute.CreateJob(
  ParamProc: TProc<TFineTuningJobParams>): TJobOut;
begin
  Result := API.Post<TJobOut, TFineTuningJobParams>('fine_tuning/jobs', ParamProc);
end;

function TFineTuningRoute.List(
  ParamProc: TProc<TFineTuningJobListParams>): TListFineTuningJobs;
begin
  Result := API.Get<TListFineTuningJobs, TFineTuningJobListParams>('fine_tuning/jobs', ParamProc);
end;

function TFineTuningRoute.Retrieve(const JobId: string): TJobOutProgress;
begin
  Result := API.GetEx<TJobOutProgress>(Format('fine_tuning/jobs/%s', [JobId]));
end;

function TFineTuningRoute.Start(const JobId: string): TJobOutProgress;
begin
  Result := API.Post<TJobOutProgress>(Format('fine_tuning/jobs/%s/start', [JobId]));
end;

{ TListFineTuningJobs }

destructor TListFineTuningJobs.Destroy;
begin
  for var Item in Data do
    Item.Free;
  inherited;
end;

{ TFineTuningJobParams }

function TFineTuningJobParams.AutoStart(
  const Value: Boolean): TFineTuningJobParams;
begin
  Result := TFineTuningJobParams(Add('auto_start', Value));
end;

function TFineTuningJobParams.Hyperparameters(
  Value: THyperparametersParams): TFineTuningJobParams;
begin
  Result := TFineTuningJobParams(Add('hyperparameters', Value.Detach));
end;

function TFineTuningJobParams.Hyperparameters(
  ParamProc: TProcRef<THyperparametersParams>): TFineTuningJobParams;
begin
  if Assigned(ParamProc) then
    begin
      var Value := THyperparametersParams.Create;
      ParamProc(Value);
      Result := Hyperparameters(Value);
    end
  else Result := Self;
end;

function TFineTuningJobParams.Integrations(
  const Value: TArray<TJobIntegrationsParams>): TFineTuningJobParams;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.Detach);
  Result := TFineTuningJobParams(Add('integrations', JSONArray));
end;

function TFineTuningJobParams.Model(const Value: string): TFineTuningJobParams;
begin
  Result := TFineTuningJobParams(Add('model', Value));
end;

function TFineTuningJobParams.Repositories(
  const Value: TArray<TRepositoryParams>): TFineTuningJobParams;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.Detach);
  Result := TFineTuningJobParams(Add('repositories', JSONArray));
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
  for var Item in FEvents do
    Item.Free;
  for var Item in FCheckpoints do
    Item.Free;
  inherited;
end;

{ TRepositoryParams }

function TRepositoryParams.Name(const Value: string): TRepositoryParams;
begin
  Result := TRepositoryParams(Add('name', Value));
end;

class function TRepositoryParams.New(
  const ParamProc: TProcRef<TRepositoryParams>): TRepositoryParams;
begin
  Result := TRepositoryParams.Create;
  if Assigned(ParamProc) then
    begin
      ParamProc(Result);
    end;
end;

function TRepositoryParams.Owner(const Value: string): TRepositoryParams;
begin
  Result := TRepositoryParams(Add('owner', Value));
end;

function TRepositoryParams.Ref(const Value: string): TRepositoryParams;
begin
  Result := TRepositoryParams(Add('ref', Value));
end;

function TRepositoryParams.Token(const Value: string): TRepositoryParams;
begin
  Result := TRepositoryParams(Add('token', Value));
end;

function TRepositoryParams.&Type(
  const Value: TRepositoryType): TRepositoryParams;
begin
  Result := TRepositoryParams(Add('type', Value.ToString));
end;

function TRepositoryParams.Weight(const Value: Double): TRepositoryParams;
begin
  Result := TRepositoryParams(Add('weight', Value));
end;

{ THyperparametersParams }

function THyperparametersParams.Epochs(
  const Value: Double): THyperparametersParams;
begin
  Result := THyperparametersParams(Add('epochs', Value));
end;

function THyperparametersParams.FimRatio(
  const Value: Double): THyperparametersParams;
begin
  Result := THyperparametersParams(Add('fim_ratio', Value));
end;

function THyperparametersParams.LearningRate(
  const Value: Double): THyperparametersParams;
begin
  Result := THyperparametersParams(Add('learning_rate', Value));
end;

class function THyperparametersParams.New(
  const ParamProc: TProcRef<THyperparametersParams>): THyperparametersParams;
begin
  Result := THyperparametersParams.Create;
  if Assigned(ParamProc) then
    begin
      ParamProc(Result);
    end;
end;

function THyperparametersParams.SeqLen(
  const Value: Integer): THyperparametersParams;
begin
  Result := THyperparametersParams(Add('seq_len', Value));
end;

function THyperparametersParams.TrainingSteps(
  const Value: Integer): THyperparametersParams;
begin
  Result := THyperparametersParams(Add('training_steps', Value));
end;

function THyperparametersParams.WarmupFraction(
  const Value: Double): THyperparametersParams;
begin
  Result := THyperparametersParams(Add('warmup_fraction', Value));
end;

function THyperparametersParams.WeightDecay(
  const Value: Double): THyperparametersParams;
begin
  Result := THyperparametersParams(Add('weight_decay', Value));
end;

{ TJobIntegrationsParams }

function TJobIntegrationsParams.ApiKey(
  const Value: string): TJobIntegrationsParams;
begin
  Result := TJobIntegrationsParams(Add('api_key', Value));
end;

function TJobIntegrationsParams.Name(
  const Value: string): TJobIntegrationsParams;
begin
  Result := TJobIntegrationsParams(Add('name', Value));
end;

class function TJobIntegrationsParams.New(
  const ParamProc: TProcRef<TJobIntegrationsParams>): TJobIntegrationsParams;
begin
  Result := TJobIntegrationsParams.Create;
  if Assigned(ParamProc) then
    begin
      ParamProc(Result);
    end;
end;

function TJobIntegrationsParams.Project(
  const Value: string): TJobIntegrationsParams;
begin
  Result := TJobIntegrationsParams(Add('project', Value));
end;

function TJobIntegrationsParams.RunName(
  const Value: string): TJobIntegrationsParams;
begin
  Result := TJobIntegrationsParams(Add('run_name', Value));
end;

function TJobIntegrationsParams.&Type(
  const Value: TFineTuningIntegrationType): TJobIntegrationsParams;
begin
  Result := TJobIntegrationsParams(Add('type', Value.ToString));
end;

{ TJobOutEvent }

destructor TJobOutEvent.Destroy;
begin
  if Assigned(FData) then
    FData.Free;
  inherited;
end;

end.

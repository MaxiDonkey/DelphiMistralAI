unit MistralAI.Types;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, MistralAI.API.Params;

type
  {$REGION 'MistralAI.Chat'}

  /// <summary>
  /// Type of message role
  /// </summary>
  TMessageRole = (
    /// <summary>
    /// System message
    /// </summary>
    system,
    /// <summary>
    /// User message
    /// </summary>
    user,
    /// <summary>
    /// Assistant message
    /// </summary>
    assistant,
    /// <summary>
    /// Function message
    /// </summary>
    tool);

  /// <summary>
  /// Helper record for the <c>TMessageRole</c> enumeration, providing utility methods for converting
  /// between <c>TMessageRole</c> values and their string representations.
  /// </summary>
  TMessageRoleHelper = record helper for TMessageRole
    /// <summary>
    /// Converts the current <c>TMessageRole</c> value to its corresponding string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TMessageRole</c> value.
    /// </returns>
    /// <remarks>
    /// <code>
    /// var Role: TMessageRole;
    /// begin
    ///   Role := TMessageRole.system;
    ///   ShowMessage(Role.ToString);  // Outputs "system"
    /// end;
    /// </code>
    /// </remarks>
    function ToString: string;
    /// <summary>
    /// Converts a string representation of a <c>TMessageRole</c> into its corresponding enumeration value.
    /// </summary>
    /// <param name="Value">
    /// The string representing a <c>TMessageRole</c>.
    /// </param>
    /// <returns>
    /// The <c>TMessageRole</c> enumeration value that corresponds to the provided string.
    /// </returns>
    /// <remarks>
    /// If the input string does not match any valid <c>TMessageRole</c> value, an exception will be raised.
    /// <code>
    /// var Role: TMessageRole;
    /// begin
    ///   Role := TMessageRoleHelper.FromString('user');  // Returns TMessageRole.user
    /// end;
    /// </code>
    /// </remarks>
    class function FromString(const Value: string): TMessageRole; static;
  end;

  /// <summary>
  /// Represents the different reasons why the processing of a request can terminate.
  /// </summary>
  TFinishReason = (
    /// <summary>
    /// API returned complete model output
    /// </summary>
    stop,
    /// <summary>
    /// Incomplete model output due to max_tokens parameter or token limit
    /// </summary>
    length_limit,
    /// <summary>
    /// model_length
    /// </summary>
    model_length,
    /// <summary>
    /// An error was encountered while processing the request
    /// </summary>
    error,
    /// <summary>
    /// A function must be invoked before further processing of the request
    /// </summary>
    tool_calls);

     /// <summary>
  /// Helper record for the <c>TFinishReason</c> enumeration, providing utility methods for conversion between string representations and <c>TFinishReason</c> values.
  /// </summary>
  TFinishReasonHelper = record helper for TFinishReason
    /// <summary>
    /// Converts the current <c>TFinishReason</c> value to its string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TFinishReason</c> value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Creates a <c>TFinishReason</c> value from its corresponding string representation.
    /// </summary>
    /// <param name="Value">
    /// The string value representing a <c>TFinishReason</c>.
    /// </param>
    /// <returns>
    /// The corresponding <c>TFinishReason</c> enumeration value for the provided string.
    /// </returns>
    /// <remarks>
    /// This method throws an exception if the input string does not match any valid <c>TFinishReason</c> values.
    /// </remarks>
    class function Create(const Value: string): TFinishReason; static;
  end;

  /// <summary>
  /// Interceptor class for converting <c>TFinishReason</c> values to and from their string representations in JSON serialization and deserialization.
  /// </summary>
  /// <remarks>
  /// This class is used to facilitate the conversion between the <c>TFinishReason</c> enum and its string equivalents during JSON processing.
  /// It extends the <c>TJSONInterceptorStringToString</c> class to override the necessary methods for custom conversion logic.
  /// </remarks>
  TFinishReasonInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts the <c>TFinishReason</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TFinishReason</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TFinishReason</c> value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string back to a <c>TFinishReason</c> value for the specified field during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>TFinishReason</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>TFinishReason</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>TFinishReason</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// Represents the type of the content: text or image_url
  /// </summary>
  TContentType = (
    /// <summary>
    /// The content is a text
    /// </summary>
    text,
    /// <summary>
    /// The content is an url or a base-64 text
    /// </summary>
    image_url
  );

  /// <summary>
  /// Helper record for the <c>TContentType</c> enumeration, providing utility methods for conversion between string representations and <c>TContentType</c> values.
  /// </summary>
  TContentTypeHelper = record Helper for TContentType
    /// <summary>
    /// Converts the current <c>TFinishReason</c> value to its string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TFinishReason</c> value.
    /// </returns>
    function ToString: string;
  end;

  /// <summary>
  /// Represents the type of tool type: only "function" available
  /// </summary>
  TToolType = (
    /// <summary>
    /// The tool calls use a function
    /// </summary>
    ttfunction
  );

  /// <summary>
  /// Helper record for the <c>TToolType</c> enumeration, providing utility methods for conversion between string representations and <c>TToolType</c> values.
  /// </summary>
  TToolTypeHelper = record Helper for TToolType
    /// <summary>
    /// Converts the current <c>TFinishReason</c> value to its string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TFinishReason</c> value.
    /// </returns>
    function ToString: string;
  end;

  {$ENDREGION}

  {$REGION 'MistralAI.FineTunings'}

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
    /// Yhe job is currently being validated
    /// </summary>
    Validating,
    /// <summary>
    /// The job has been validated
    /// </summary>
    Validated,
    /// <summary>
    /// The job is currently running.
    /// </summary>
    Running,
    /// <summary>
    /// The validation failed.
    /// </summary>
    Failed_validation,
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
    /// <summary>
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

  TRepositoryType = (
    github
  );

  /// <summary>
  /// Provides helper methods for the TRepositoryType enum.
  /// </summary>
  /// <remarks>
  /// Includes methods to convert enum values to strings and create enum values from strings.
  /// </remarks>
  TRepositoryTypeHelper = record Helper for TRepositoryType
    /// <summary>
    /// Converts the TRepositoryType value to its string representation.
    /// </summary>
    /// <returns>
    /// The string representation of the TRepositoryType value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Creates a TRepositoryType enum value from a string.
    /// </summary>
    /// <param name="Value">
    /// The string representation of the TRepositoryType.
    /// </param>
    /// <returns>
    /// The corresponding TRepositoryType enum value.
    /// </returns>
    class function Create(const Value: string): TRepositoryType; static;
  end;

  /// <summary>
  /// JSON interceptor for converting TRepositoryType enum values to strings and vice versa during JSON serialization and deserialization.
  /// </summary>
  TRepositoryTypeInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts a TRepositoryType enum value to its string representation for JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to convert.
    /// </param>
    /// <param name="Field">
    /// The name of the field to convert.
    /// </param>
    /// <returns>
    /// The string representation of the TRepositoryType enum value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string from JSON deserialization back to a TRepositoryType enum value.
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
  /// Interceptor class for converting <c>args</c> and <c>response</c> values into JSON string format in JSON deserialization.
  /// </summary>
  /// <remarks>
  /// This class is used to facilitate the conversion between the <c>args</c>, <c>response</c> and theirs string equivalent during JSON processing.
  /// It extends the <c>TJSONInterceptorStringToString</c> class to override the necessary methods for custom conversion logic.
  /// </remarks>
  TMetadataInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// When JSON deserialization, converts <c>args</c>, <c>response<c/> values into JSON string to retrieve arguments made by the tool.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>input</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>args</c> or <c>response</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>args</c> or <c>response</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  {$ENDREGION}

  {$REGION 'MistralAI.Codestral'}

  /// <summary>
  /// Represents the different reasons why the processing of a request can terminate.
  /// </summary>
  TCodestralFinishReason = (
    /// <summary>
    /// API returned complete model output
    /// </summary>
    csstop,
    /// <summary>
    /// Incomplete model output due to max_tokens parameter or token limit
    /// </summary>
    cslength_limite,
    /// <summary>
    /// model_length
    /// </summary>
    csmodel_length,
    /// <summary>
    /// An error was encountered while processing the request
    /// </summary>
    cserror,
    /// <summary>
    /// A function must be invoked before further processing of the request
    /// </summary>
    cstool_calls);

  /// <summary>
  /// Provides helper methods for the TCodestralFinishReason enumeration.
  /// </summary>
  TCodestralFinishReasonHelper = record helper for TCodestralFinishReason
    /// <summary>
    /// Returns the string representation of the TCodestralFinishReason value.
    /// </summary>
    function ToString: string;
    /// <summary>
    /// Creates a TCodestralFinishReason value from a given string.
    /// </summary>
    /// <param name="Value">The string representation of the TCodestralFinishReason.</param>
    /// <returns>A TCodestralFinishReason corresponding to the provided string.</returns>
    class function Create(const Value: string): TCodestralFinishReason; static;
  end;

  /// <summary>
  /// Interceptor class for converting and reverting TCodestralFinishReason values to and from strings in JSON.
  /// </summary>
  TCodestralFinishReasonInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts a TCodestralFinishReason value to its string representation for JSON serialization.
    /// </summary>
    /// <param name="Data">The object containing the field to be converted.</param>
    /// <param name="Field">The name of the field to be converted.</param>
    /// <returns>The string representation of the TCodestralFinishReason value.</returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Reverts a string representation back to a TCodestralFinishReason value during JSON deserialization.
    /// </summary>
    /// <param name="Data">The object containing the field to be reverted.</param>
    /// <param name="Field">The name of the field to be reverted.</param>
    /// <param name="Arg">
    /// The string representation of the TCodestralFinishReason value.
    ///</param>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  {$ENDREGION}

  {$REGION 'MistralAI.Files'}

  /// <summary>
  /// Specifies the intended purpose of the uploaded file.
  /// </summary>
  /// <remarks>
  /// Enum :
  /// <para>
  /// finetune, batch
  /// </para>
  /// </remarks>
  TFilePurpose = (
    /// <summary>
    /// The file will be used for fine-tuning.
    /// </summary>
    finetune,
    /// <summary>
    /// The file will be used for batch operation
    /// </summary>
    batch
  );

  /// <summary>
  /// Helper methods for <c>TFilePurpose</c> enumeration.
  /// </summary>
  TFilePurposeHelper = record helper for TFilePurpose
    /// <summary>
    /// Converts the <c>TFilePurpose</c> value to its string representation.
    /// </summary>
    /// <returns>
    /// A string representing the <c>TFilePurpose</c> value.
    /// </returns>
    /// <remarks>
    /// For example:
    /// <code>
    /// var
    ///   Purpose: TFilePurpose;
    /// begin
    ///   Purpose := TFilePurpose.finetune;
    ///   ShowMessage(Purpose.ToString); // Outputs 'fine-tune'
    /// end;
    /// </code>
    /// </remarks>
    function ToString: string;
    /// <summary>
    /// Creates a <c>TFilePurpose</c> from its string representation.
    /// </summary>
    /// <param name="Value">
    /// The string representation of the <c>TFilePurpose</c>.
    /// </param>
    /// <returns>
    /// The <c>TFilePurpose</c> corresponding to the specified string.
    /// </returns>
    /// <exception cref="Exception">
    /// Raised if the string does not correspond to any <c>TFilePurpose</c> value.
    /// </exception>
    /// <remarks>
    /// For example:
    /// <code>
    /// var
    ///   Purpose: TFilePurpose;
    /// begin
    ///   Purpose := TFilePurposeHelper.Create('fine-tune');
    ///   // Purpose now equals TFilePurpose.finetune
    /// end;
    /// </code>
    /// </remarks>
    class function Create(const Value: string): TFilePurpose; static;
  end;

  /// <summary>
  /// JSON interceptor to convert <c>TFilePurpose</c> to and from its string representation during JSON serialization.
  /// </summary>
  TFilePurposeInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts the <c>TFilePurpose</c> field value to its string representation.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field.
    /// </param>
    /// <param name="Field">
    /// The name of the field to convert.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TFilePurpose</c> value.
    /// </returns>
    /// <remarks>
    /// This method is used internally during JSON serialization.
    /// </remarks>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string to the <c>TFilePurpose</c> field value during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field.
    /// </param>
    /// <param name="Field">
    /// The name of the field to convert.
    /// </param>
    /// <param name="Arg">
    /// The string value to convert to <c>TFilePurpose</c>.
    /// </param>
    /// <remarks>
    /// This method is used internally during JSON deserialization.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// Specifies the sample_type of the file.
  /// </summary>
  /// <remarks>
  /// Enum :
  /// <para>
  /// pretrain, instruct, batch_request, batch_result, batch_error
  /// </para>
  /// </remarks>
  TSampleType = (
    pretrain,
    instruct,
    batch_request,
    batch_result,
    batch_error
  );

  /// <summary>
  /// Helper methods for <c>TSampleType</c> enumeration.
  /// </summary>
  TSampleTypeHelper = record Helper for TSampleType
    /// <summary>
    /// Converts the <c>TSampleType</c> value to its string representation.
    /// </summary>
    /// <returns>
    /// A string representing the <c>TSampleType</c> value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Creates a <c>TSampleType</c> from its string representation.
    /// </summary>
    /// <param name="Value">
    /// The string representation of the <c>TSampleType</c>.
    /// </param>
    /// <returns>
    /// The <c>TSampleType</c> corresponding to the specified string.
    /// </returns>
    /// <exception cref="Exception">
    /// Raised if the string does not correspond to any <c>TSampleType</c> value.
    /// </exception>
    class function Create(const Value: string): TSampleType; static;
  end;

  /// <summary>
  /// JSON interceptor to convert <c>TSampleType</c> to and from its string representation during JSON serialization.
  /// </summary>
  TSampleTypeInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts the <c>TSampleType</c> field value to its string representation.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field.
    /// </param>
    /// <param name="Field">
    /// The name of the field to convert.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TSampleType</c> value.
    /// </returns>
    /// <remarks>
    /// This method is used internally during JSON serialization.
    /// </remarks>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string to the <c>TSampleType</c> field value during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field.
    /// </param>
    /// <param name="Field">
    /// The name of the field to convert.
    /// </param>
    /// <param name="Arg">
    /// The string value to convert to <c>TSampleType</c>.
    /// </param>
    /// <remarks>
    /// This method is used internally during JSON deserialization.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// Specifies the source of the file.
  /// </summary>
  /// <remarks>
  /// Enum :
  /// <para>
  /// upload, repository, mistral
  /// </para>
  /// </remarks>
  TSourceType = (
    upload,
    repository,
    mistral
  );

  TSourceTypeHelper = record Helper for TSourceType
    /// <summary>
    /// Converts the <c>TSourceType</c> value to its string representation.
    /// </summary>
    /// <returns>
    /// A string representing the <c>TSourceType</c> value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Creates a <c>TSourceType</c> from its string representation.
    /// </summary>
    /// <param name="Value">
    /// The string representation of the <c>TSourceType</c>.
    /// </param>
    /// <returns>
    /// The <c>TSourceType</c> corresponding to the specified string.
    /// </returns>
    /// <exception cref="Exception">
    /// Raised if the string does not correspond to any <c>TSourceType</c> value.
    /// </exception>
    class function Create(const Value: string): TSourceType; static;
  end;

  /// <summary>
  /// JSON interceptor to convert <c>TSourceType</c> to and from its string representation during JSON serialization.
  /// </summary>
  TSourceTypeInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts the <c>TSourceType</c> field value to its string representation.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field.
    /// </param>
    /// <param name="Field">
    /// The name of the field to convert.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TSourceType</c> value.
    /// </returns>
    /// <remarks>
    /// This method is used internally during JSON serialization.
    /// </remarks>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string to the <c>TSourceType</c> field value during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field.
    /// </param>
    /// <param name="Field">
    /// The name of the field to convert.
    /// </param>
    /// <param name="Arg">
    /// The string value to convert to <c>TSourceType</c>.
    /// </param>
    /// <remarks>
    /// This method is used internally during JSON deserialization.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  {$ENDREGION}

  {$REGION 'MistralAI.Functions.Tools'}

  /// <summary>
  /// Defines the model's function call behavior. This type determines whether the model generates
  /// a response, calls a function, or decides between the two.
  /// </summary>
  TToolChoice = (
    /// <summary>
    /// The model won't call a function and will generate a message instead
    /// </summary>
    none,
    /// <summary>
    /// The model can choose to either generate a message or call a function
    /// </summary>
    auto,
    /// <summary>
    /// The model is forced to call a function
    /// </summary>
    any,
    /// <summary>
    /// The model is required to call a function
    /// </summary>
    required);

  /// <summary>
  /// A helper record for the <c>TToolChoice</c> type, providing additional functionality.
  /// </summary>
  TToolChoiceHelper = record helper for TToolChoice
    /// <summary>
    /// Converts the <c>TToolChoice</c> value to its string representation.
    /// </summary>
    /// <returns>A string representing the current <c>TToolChoice</c> value.</returns>
    function ToString: string;
  end;

  {$ENDREGION}

  {$REGION 'MistralAI.Batch'}

  /// <summary>
  /// Represents the different status of a batch.
  /// </summary>
  TBatchStatus = (
    /// <summary>
    /// The batch is waiting to be processed. It is placed on the queue.
    /// </summary>
    BSQueued,
    /// <summary>
    /// The batch is being processed.
    /// </summary>
    BSRunning,
    /// <summary>
    /// Batch processing completed successfully.
    /// </summary>
    BSSuccess,
    /// <summary>
    /// Batch processing failed.
    /// </summary>
    BSFailed,
    /// <summary>
    /// The execution deadline has been reached.
    /// </summary>
    BSTimeout_exceeded,
    /// <summary>
    /// A request to abandon batch processing has been submitted.
    /// </summary>
    BSCancellation_requested,
    /// <summary>
    /// Batch processing has been aborted.
    /// </summary>
    BSCancelled
  );

  /// <summary>
  /// Provides helper methods for the TBatchStatus enumeration.
  /// </summary>
  TBatchStatusHelper = record Helper for TBatchStatus
    /// <summary>
    /// Returns the string representation of the TBatchStatus value.
    /// </summary>
    function ToString: string;
    /// <summary>
    /// Creates a TBatchStatus value from a given string.
    /// </summary>
    /// <param name="Value">The string representation of the TBatchStatus.</param>
    /// <returns>A TBatchStatus corresponding to the provided string.</returns>
    class function Create(const Value: string): TBatchStatus; static;
  end;

  /// <summary>
  /// Interceptor class for converting and reverting TBatchStatus values to and from strings in JSON.
  /// </summary>
  TBatchStatusInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts a TBatchStatus value to its string representation for JSON serialization.
    /// </summary>
    /// <param name="Data">The object containing the field to be converted.</param>
    /// <param name="Field">The name of the field to be converted.</param>
    /// <returns>The string representation of the TBatchStatus value.</returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Reverts a string representation back to a TBatchStatus value during JSON deserialization.
    /// </summary>
    /// <param name="Data">The object containing the field to be reverted.</param>
    /// <param name="Field">The name of the field to be reverted.</param>
    /// <param name="Arg">
    /// The string representation of the TBatchStatus value.
    ///</param>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// Endpoint for batch job.
  /// </summary>
  /// <remarks>
  /// Enum : epChatCompletion, epEmbeddings, epFimCompletions, epModeration
  /// <para>
  /// respectively for :
  /// </para>
  /// <para>
  /// - epChatCompletion : /v1/chat/completions
  /// </para>
  /// <para>
  /// - epEmbeddings : /v1/embeddings
  /// </para>
  /// <para>
  /// - epFimCompletions : /v1/fim/completions
  /// </para>
  /// <para>
  /// - epModeration : /v1/moderations
  /// </para>
  /// </remarks>
  TEndPointType = (
    /// <summary>
    /// /v1/chat/completions
    /// </summary>
    epChatCompletion,
    /// <summary>
    /// /v1/embeddings
    /// </summary>
    epEmbeddings,
    /// <summary>
    /// /v1/fim/completions
    /// </summary>
    epFimCompletions,
    /// <summary>
    /// /v1/moderations
    /// </summary>
    epModeration
  );

  /// <summary>
  /// Provides helper methods for the TEndPointType enumeration.
  /// </summary>
  TEndPointTypeHelper = record Helper for TEndPointType
    /// <summary>
    /// Returns the string representation of the TEndPointType value.
    /// </summary>
    /// <remarks>
    /// Enum : epChatCompletion, epEmbeddings, epFimCompletions, epModeration
    /// <para>
    /// respectively for :
    /// </para>
    /// <para>
    /// - epChatCompletion : /v1/chat/completions
    /// </para>
    /// <para>
    /// - epEmbeddings : /v1/embeddings
    /// </para>
    /// <para>
    /// - epFimCompletions : /v1/fim/completions
    /// </para>
    /// <para>
    /// - epModeration : /v1/moderations
    /// </para>
    /// </remarks>
    function ToString: string;
    /// <summary>
    /// Creates a TEndPointType value from a given string.
    /// </summary>
    /// <param name="Value">The string representation of the TEndPointType.</param>
    /// <returns>A TEndPointType corresponding to the provided string.</returns>
    class function Create(const Value: string): TEndPointType; static;
  end;

  /// <summary>
  /// Interceptor class for converting and reverting TEndPointType values to and from strings in JSON.
  /// </summary>
  TEndPointTypeInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts a TEndPointType value to its string representation for JSON serialization.
    /// </summary>
    /// <param name="Data">The object containing the field to be converted.</param>
    /// <param name="Field">The name of the field to be converted.</param>
    /// <returns>The string representation of the TEndPointType value.</returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Reverts a string representation back to a TEndPointType value during JSON deserialization.
    /// </summary>
    /// <param name="Data">The object containing the field to be reverted.</param>
    /// <param name="Field">The name of the field to be reverted.</param>
    /// <param name="Arg">
    /// The string representation of the TEndPointType value.
    ///</param>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  {$ENDREGION}

  {$REGION 'MistralAI.Classifiers'}

  resourcestring
    SexualCategory = 'Sexual';
    HateAndDiscriminationCategory = 'Hate and discrimination';
    ViolenceAndThreatsCategory = 'Violence and threats';
    DangerousAndCriminalContentCategory = 'Dangerous and criminal content';
    SelfharmCategory = 'Selfharm';
    HealthCategory = 'Health';
    FinancialCategory = 'Financial';
    LawCategory = 'Law';
    PiiCategory = 'Pii';

  var
    Classifiers: TArray<string> =
      [SexualCategory, HateAndDiscriminationCategory, ViolenceAndThreatsCategory,
       DangerousAndCriminalContentCategory, SelfharmCategory, HealthCategory,
       FinancialCategory, LawCategory, PiiCategory];

  {$ENDREGION}

implementation

uses
  System.StrUtils, Rest.Json, System.Rtti;

{ TMessageRoleHelper }

class function TMessageRoleHelper.FromString(const Value: string): TMessageRole;
begin
  var index := IndexStr(Value.ToLower, ['system', 'user', 'assistant', 'tool']);
  if index = -1 then
    raise Exception.Create('Invalid message role value.');
  Result := TMessageRole(index);
end;

function TMessageRoleHelper.ToString: string;
begin
  case Self of
    system:
      Exit('system');
    user:
      Exit('user');
    assistant:
      Exit('assistant');
    tool:
      Exit('tool');
  end;
end;

{ TFinishReasonHelper }

class function TFinishReasonHelper.Create(const Value: string): TFinishReason;
begin
  var index := IndexStr(Value.ToLower, ['stop', 'length', 'model_length', 'error', 'tool_calls']);
  if index = -1 then
    raise Exception.Create('Invalid finish reason value.');
  Result := TFinishReason(index);
end;

function TFinishReasonHelper.ToString: string;
begin
  case Self of
    stop:
      Exit('stop');
    length_limit:
      Exit('length');
    model_length:
      Exit('model_length');
    error:
      Exit('error');
    tool_calls:
      Exit('tool_calls');
  end;
end;

{ TFinishReasonInterceptor }

function TFinishReasonInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TFinishReason>.ToString;
end;

procedure TFinishReasonInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TFinishReason.Create(Arg)));
end;

{ TContentTypeHelper }

function TContentTypeHelper.ToString: string;
begin
  case Self of
    text:
      Exit('text');
    image_url:
      Exit('image_url');
  end;
end;

{ TToolTypeHelper }

function TToolTypeHelper.ToString: string;
begin
  case Self of
    ttfunction:
      Exit('function');
  end;
end;

{ TFineTuningJobStatusHelper }

class function TFineTuningJobStatusHelper.Create(
  const Value: string): TFineTuningJobStatus;
begin
  var index := IndexStr(Value.ToUpper, [
    'QUEUED', 'STARTED', 'VALIDATING', 'VALIDATED', 'RUNNING', 'FAILED_VALIDATION', 'FAILED',
    'SUCCESS', 'CANCELLED', 'CANCELLATION_REQUESTED']);
  if index = -1 then
    raise Exception.Create('Invalid fine-tuning job status value.');
  Result := TFineTuningJobStatus(index);
end;

function TFineTuningJobStatusHelper.ToString: string;
begin
  case Self of
    Queued:
      Exit('QUEUED');
    Started:
      Exit('STARTED');
    Validating:
      Exit('VALIDATING');
    Validated:
      Exit('VALIDATED');
    Running:
      Exit('RUNNING');
    Failed_validation:
      Exit('FAILED_VALIDATION');
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
  var index := IndexStr(Value.ToLower, ['job']);
  if index = -1 then
    raise Exception.Create('Invalid fine-tuning data object value.');
  Result := TFineTuningDataObjectKind(index);
end;

function TFineTuningDataObjectKindHelper.ToString: string;
begin
  case Self of
    Job:
      Exit('job');
  end;
end;

{ TFineTuningDataObjectKindInterceptor }

function TFineTuningDataObjectKindInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TFineTuningDataObjectKind>.ToString;
end;

procedure TFineTuningDataObjectKindInterceptor.StringReverter(Data: TObject;
  Field, Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TFineTuningDataObjectKind.Create(Arg)));
end;

{ TFineTuningIntegrationTypeHelper }

class function TFineTuningIntegrationTypeHelper.Create(
  const Value: string): TFineTuningIntegrationType;
begin
  case IndexStr(Value.ToLower, ['wandb']) of
    0 :
      Exit(Wandb);
    else
      raise Exception.Create('Invalid fine-tuning integration value.');
  end;
end;

function TFineTuningIntegrationTypeHelper.ToString: string;
begin
  case self of
    Wandb:
      Exit('wandb');
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
  var index := IndexStr(Value.ToLower, ['list']);
  if index = -1 then
    raise Exception.Create('Invalid Fine tuning object value.');
  Result := TFineTuningObjectKind(index);
end;

function TFineTuningObjectKindHelper.ToString: string;
begin
  case self of
    List:
      Exit('list');
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

{ TCodestralFinishReasonHelper }

class function TCodestralFinishReasonHelper.Create(
  const Value: string): TCodestralFinishReason;
begin
  var index := IndexStr(Value.ToLower, ['stop', 'length', 'model_length', 'error', 'tool_calls']);
  if index = -1 then
    raise Exception.Create('Invalid codestral finish reason value.');
  Result := TCodestralFinishReason(index);
end;

function TCodestralFinishReasonHelper.ToString: string;
begin
  case Self of
    csstop:
      Exit('stop');
    cslength_limite:
      Exit('length');
    csmodel_length:
      Exit('model_length');
    cserror:
      Exit('error');
    cstool_calls:
      Exit('tool_calls');
  end;
end;

{ TCodestralFinishReasonInterceptor }

function TCodestralFinishReasonInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TCodestralFinishReason>.ToString;
end;

procedure TCodestralFinishReasonInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TCodestralFinishReason.Create(Arg)));
end;

{ TFilePurposeHelper }

class function TFilePurposeHelper.Create(const Value: string): TFilePurpose;
begin
  var index := IndexStr(Value.ToLower, ['fine-tune', 'batch']);
  if index = - 1 then
    raise Exception.Create('Invalid files purpose value.');
  Result := TFilePurpose(index);
end;

function TFilePurposeHelper.ToString: string;
begin
  case self of
    FineTune:
      Exit('fine-tune');
    batch:
      Exit('batch');
  end;
end;

{ TFilePurposeInterceptor }

function TFilePurposeInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TFilePurpose>.ToString;
end;

procedure TFilePurposeInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TFilePurpose.Create(Arg)));
end;

{ TToolChoiceHelper }

function TToolChoiceHelper.ToString: string;
begin
  case Self of
    auto:
      Exit('auto');
    any:
      Exit('any');
    required:
      Exit('required');
    else
      Exit('none');
  end;
end;

{ TSampleTypeHelper }

class function TSampleTypeHelper.Create(const Value: string): TSampleType;
begin
  var index := IndexStr(Value.ToLower, [
    'pretrain', 'instruct', 'batch_request', 'batch_result', 'batch_error']);
  if index = -1 then
    raise Exception.Create('Invalid file sample value.');
  Result := TSampleType(index);
end;

function TSampleTypeHelper.ToString: string;
begin
  case Self of
    pretrain:
      Exit('pretrain');
    instruct:
      Exit('instruct');
    batch_request:
      Exit('batch_request');
    batch_result:
      Exit('batch_result');
    batch_error:
      Exit('batch_error');
  end;
end;

{ TSourceTypeHelper }

class function TSourceTypeHelper.Create(const Value: string): TSourceType;
begin
  var index := IndexStr(Value.ToLower, ['upload', 'repository', 'mistral']);
  if index = -1 then
    raise Exception.Create('Invalid file source value.');
  Result := TSourceType(index);
end;

function TSourceTypeHelper.ToString: string;
begin
  case Self of
    upload:
      Exit('upload');
    repository:
      Exit('repository');
    mistral:
      Exit('mistral');
  end;
end;

{ TSampleTypeInterceptor }

function TSampleTypeInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TSampleType>.ToString;
end;

procedure TSampleTypeInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TSampleType.Create(Arg)));
end;

{ TSourceTypeInterceptor }

function TSourceTypeInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TSourceType>.ToString;
end;

procedure TSourceTypeInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TSourceType.Create(Arg)));
end;

{ TRepositoryTypeHelper }

class function TRepositoryTypeHelper.Create(
  const Value: string): TRepositoryType;
begin
  var index := IndexStr(Value.ToLower, ['github']);
  if index = -1 then
    raise Exception.Create(' Invalid repository value.');
  Result := TRepositoryType(index);
end;

function TRepositoryTypeHelper.ToString: string;
begin
  case Self of
    github:
      Exit('github');
  end;
end;

{ TRepositoryTypeInterceptor }

function TRepositoryTypeInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TRepositoryType>.ToString;
end;

procedure TRepositoryTypeInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TRepositoryType.Create(Arg)));
end;

{ TBatchStatusHelper }

class function TBatchStatusHelper.Create(const Value: string): TBatchStatus;
begin
  var index := IndexStr(Value.ToUpper, [
    'QUEUED', 'RUNNING', 'SUCCESS', 'FAILED', 'TIMEOUT_EXCEEDED', 'CANCELLATION_REQUESTED', 'CANCELLED']);
  if index = -1 then
    raise Exception.Create('Invalid batch status value.');
  Result := TBatchStatus(index);
end;

function TBatchStatusHelper.ToString: string;
begin
  case Self of
    BSQueued:
      Exit('QUEUED');
    BSRunning:
      Exit('RUNNING');
    BSSuccess:
      Exit('SUCCESS');
    BSFailed:
      Exit('FAILED');
    BSTimeout_exceeded:
      Exit('TIMEOUT_EXCEEDED');
    BSCancellation_requested:
      Exit('CANCELLATION_REQUESTED');
    BSCancelled:
      Exit('CANCELLED');
  end;
end;

{ TBatchStatusInterceptor }

function TBatchStatusInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TBatchStatus>.ToString;
end;

procedure TBatchStatusInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TBatchStatus.Create(Arg)));
end;

{ TEndPointTypeHelper }

class function TEndPointTypeHelper.Create(const Value: string): TEndPointType;
begin
  var index := IndexStr(Value.ToLower, [
    '/v1/chat/completions', '/v1/embeddings', '/v1/fim/completions', '/v1/moderations']);
  if index = -1 then
    raise Exception.Create('Invalid endpoint value.');
  Result := TEndPointType(index);
end;

function TEndPointTypeHelper.ToString: string;
begin
  case Self of
    epChatCompletion:
      Exit('/v1/chat/completions');
    epEmbeddings:
      Exit('/v1/embeddings');
    epFimCompletions:
      Exit('/v1/fim/completions');
    epModeration:
      Exit('/v1/moderations');
  end;
end;

{ TEndPointTypeInterceptor }

function TEndPointTypeInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TEndPointType>.ToString;
end;

procedure TEndPointTypeInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TEndPointType.Create(Arg)));
end;

{ TMetadataInterceptor }

procedure TMetadataInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  Arg := Format('{%s}', [Trim(Arg.Replace('`', '"').Replace(#10, ''))]);
  while Arg.Contains(', ') do Arg := Arg.Replace(', ', ',');
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, Arg.Replace(',', ', '));
end;

end.

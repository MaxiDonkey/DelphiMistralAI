unit MistralAI.Types;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, MistralAI.API.Params, System.JSON, REST.JsonReflect;

const
  ReasoningEnglishInstructions =
    'An end user will ask you to solve a task. Begin by drafting your thought process (inner monologue) until you arrive at the final answer. Then write a summary of your reflections (i.e., concise but containing all essential steps needed to reach the conclusion). Use Markdown to format your response. Write both your thoughts and your summary in the same language as the task presented by the user. NEVER use \boxed{} in your answer.'+ #10 +
    'Your thought process must follow the model below:'+ #10 +
    '<think>'+ #10 +
    'Your thoughts and/or your draft, as if working out a rough exercise on scrap paper. Feel free to be as casual and as long-winded as you like until you’re certain you can arrive at the correct answer.'+ #10 +
    '</think>'+ #10 +
    'Provide here a concise summary that reflects your reasoning and presents a clear final answer to the user. Do not indicate that it is a summary.';

{$SCOPEDENUMS ON}

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
    constructor Create(const Value: string);
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
  end;

  TMessageRoleInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts the <c>TMessageRole</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TMessageRole</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TMessageRole</c> value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string back to a <c>TMessageRole</c> value for the specified field during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>TMessageRole</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>TMessageRole</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>TMessageRole</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
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
    length,
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
    constructor Create(const Value: string);
    /// <summary>
    /// Converts the current <c>TFinishReason</c> value to its string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TFinishReason</c> value.
    /// </returns>
    function ToString: string;
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
    text,
    image_url,
    reference,
    document_url,
    input_audio,
    thinking
  );

  /// <summary>
  /// Helper record for the <c>TContentType</c> enumeration, providing utility methods for conversion between string representations and <c>TContentType</c> values.
  /// </summary>
  TContentTypeHelper = record Helper for TContentType
    constructor Create(const Value: string);
    /// <summary>
    /// Converts the current <c>TFinishReason</c> value to its string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TFinishReason</c> value.
    /// </returns>
    function ToString: string;
  end;

  TContentTypeInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts the <c>TContentType</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TContentType</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TContentType</c> value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string back to a <c>TContentType</c> value for the specified field during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>TContentType</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>TContentType</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>TFinishReason</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// Represents the type of tool type: only "function" available
  /// </summary>
  TToolType = (
    web_search_premium,
    web_search,
    image_generation,
    &function,
    document_library,
    code_interpreter
  );

  /// <summary>
  /// Helper record for the <c>TToolType</c> enumeration, providing utility methods for conversion between string representations and <c>TToolType</c> values.
  /// </summary>
  TToolTypeHelper = record Helper for TToolType
    constructor Create(const Value: string);
    /// <summary>
    /// Converts the current <c>TFinishReason</c> value to its string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TFinishReason</c> value.
    /// </returns>
    function ToString: string;
  end;

  TResponseFormatType = (
    text,
    json_object,
    json_schema
  );

  TResponseFormatTypeHelper = record Helper for TResponseFormatType
    constructor Create(const Value: string);
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
    constructor Create(const Value: string);
    /// <summary>
    /// Converts the TFineTuningJobStatus value to its string representation.
    /// </summary>
    /// <returns>
    /// The string representation of the TFineTuningJobStatus value.
    /// </returns>
    function ToString: string;
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
    constructor Create(const Value: string);
    /// <summary>
    /// Converts the TFineTuningDataObjectKind value to its string representation.
    /// </summary>
    /// <returns>
    /// The string representation of the TFineTuningDataObjectKind value.
    /// </returns>
    function ToString: string;
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
    constructor Create(const Value: string);
    /// <summary>
    /// Converts the TFineTuningIntegrationType value to its string representation.
    /// </summary>
    /// <returns>
    /// The string representation of the TFineTuningIntegrationType value.
    /// </returns>
    function ToString: string;
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
    constructor Create(const Value: string);
    /// <summary>
    /// Converts the TFineTuningObjectKind value to its string representation.
    /// </summary>
    /// <returns>
    /// The string representation of the TFineTuningObjectKind value.
    /// </returns>
    function ToString: string;
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
    constructor Create(const Value: string);
    /// <summary>
    /// Converts the TRepositoryType value to its string representation.
    /// </summary>
    /// <returns>
    /// The string representation of the TRepositoryType value.
    /// </returns>
    function ToString: string;
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
    stop,
    /// <summary>
    /// Incomplete model output due to max_tokens parameter or token limit
    /// </summary>
    length_limite,
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
  /// Provides helper methods for the TCodestralFinishReason enumeration.
  /// </summary>
  TCodestralFinishReasonHelper = record helper for TCodestralFinishReason
    constructor Create(const Value: string);
    /// <summary>
    /// Returns the string representation of the TCodestralFinishReason value.
    /// </summary>
    function ToString: string;
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
    batch,
    /// <summary>
    /// The file will be used for ocr operation
    /// </summary>
    ocr,
    audio,
    image_generation
  );

  /// <summary>
  /// Helper methods for <c>TFilePurpose</c> enumeration.
  /// </summary>
  TFilePurposeHelper = record helper for TFilePurpose
    constructor Create(const Value: string);
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
    batch_error,
    audio_input,
    image_generation,
    ocr_input
  );

  /// <summary>
  /// Helper methods for <c>TSampleType</c> enumeration.
  /// </summary>
  TSampleTypeHelper = record Helper for TSampleType
    constructor Create(const Value: string);
    /// <summary>
    /// Converts the <c>TSampleType</c> value to its string representation.
    /// </summary>
    /// <returns>
    /// A string representing the <c>TSampleType</c> value.
    /// </returns>
    function ToString: string;
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
    constructor Create(const Value: string);
    /// <summary>
    /// Converts the <c>TSourceType</c> value to its string representation.
    /// </summary>
    /// <returns>
    /// A string representing the <c>TSourceType</c> value.
    /// </returns>
    function ToString: string;
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
    constructor Create(const Value: string);
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
    Queued,
    /// <summary>
    /// The batch is being processed.
    /// </summary>
    Running,
    /// <summary>
    /// Batch processing completed successfully.
    /// </summary>
    Success,
    /// <summary>
    /// Batch processing failed.
    /// </summary>
    Failed,
    /// <summary>
    /// The execution deadline has been reached.
    /// </summary>
    Timeout_exceeded,
    /// <summary>
    /// A request to abandon batch processing has been submitted.
    /// </summary>
    Cancellation_requested,
    /// <summary>
    /// Batch processing has been aborted.
    /// </summary>
    Cancelled
  );

  /// <summary>
  /// Provides helper methods for the TBatchStatus enumeration.
  /// </summary>
  TBatchStatusHelper = record Helper for TBatchStatus
    constructor Create(const Value: string);
    /// <summary>
    /// Returns the string representation of the TBatchStatus value.
    /// </summary>
    function ToString: string;
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
    ChatCompletion,
    /// <summary>
    /// /v1/embeddings
    /// </summary>
    Embeddings,
    /// <summary>
    /// /v1/fim/completions
    /// </summary>
    FimCompletions,
    /// <summary>
    /// /v1/moderations
    /// </summary>
    Moderation
  );

  /// <summary>
  /// Provides helper methods for the TEndPointType enumeration.
  /// </summary>
  TEndPointTypeHelper = record Helper for TEndPointType
    constructor Create(const Value: string);
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

  {$REGION 'MistralAI.Conversations'}

  THandoffExecutionType = (
    client,
    server
  );

  THandoffExecutionTypeHelper = record Helper for THandoffExecutionType
    constructor Create(const Value: string);
    function ToString: string;
  end;

  TContentChunkType = (
    text,
    image_url,
    tool_file,
    document_url,
    tool_reference,
    thinking
  );

  TContentChunkTypeHelper = record Helper for TContentChunkType
    constructor Create(const Value: string);
    function ToString: string;
  end;

  TContentChunkTypeInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts the <c>TContentChunkType</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TContentChunkType</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TContentChunkType</c> value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string back to a <c>TContentChunkType</c> value for the specified field during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>TContentChunkType</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>TContentChunkType</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>TContentChunkType</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TConversationTool = (
    web_search,
    web_search_premium,
    code_interpreter,
    image_generation,
    document_library
  );

  TConversationToolHelper = record Helper for TConversationTool
    constructor Create(const Value: string);
    function ToString: string;
  end;

  TConversationToolInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts the <c>TConversationTool</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TConversationTool</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TConversationTool</c> value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string back to a <c>TConversationTool</c> value for the specified field during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>TConversationTool</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>TConversationTool</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>TConversationTool</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TChunkEvent = (
    conversation_response_started,
    conversation_response_done,
    conversation_response_error,
    message_output_delta,
    tool_execution_started,
    tool_execution_done,
    agent_handoff_started,
    agent_handoff_done,
    function_call_delta
  );

  TChunkEventHelper = record Helper for TChunkEvent
    constructor Create(const Value: string);
    function ToString: string;
  end;

  TChunkEventInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts the <c>TChunkEvent</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TChunkEvent</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TChunkEvent</c> value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string back to a <c>TChunkEvent</c> value for the specified field during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>TChunkEvent</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>TChunkEvent</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>TChunkEvent</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TConversatonEvent = (
    message_input,
    message_output,
    tool_execution,
    function_call,
    agent_handoff
  );

  TConversatonEventHelper = record Helper for TConversatonEvent
    constructor Create(const Value: string);
    function ToString: string;
  end;

  TConversatonEventInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts the <c>TConversatonEvent</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TConversatonEvent</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TConversatonEvent</c> value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string back to a <c>TConversatonEvent</c> value for the specified field during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>TConversatonEvent</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>TConversatonEvent</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>TConversatonEvent</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  {$SCOPEDENUMS OFF}

  TReasoningInstruction = (
    Custom,
    Default
  );

  {$SCOPEDENUMS ON}

  TReasoningInstructionHelper = record Helper for TReasoningInstruction
    function ToString: string;
  end;

  {$ENDREGION}

  {$REGION 'MistralAI.Libraries.Access'}

  TLevelType = (
    viewer,
    editor
  );

  TLevelTypeHelper = record Helper for TLevelType
    constructor Create(const Value: string);
    function ToString: string;
  end;

  TShareWithType = (
    user,
    workspace,
    org
  );

  TShareWithTypeHelper = record Helper for TShareWithType
    constructor Create(const Value: string);
    function ToString: string;
  end;

  TShareWithTypeInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts the <c>TShareWithType</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TShareWithType</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TShareWithType</c> value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string back to a <c>TShareWithType</c> value for the specified field during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>TShareWithType</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>TShareWithType</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>TShareWithType</c> value and assigns it to the specified field in the object.
    /// </remarks>
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
  System.StrUtils, Rest.Json, System.Rtti, System.TypInfo, System.Classes;

type
  TEnumValueRecovery = class
    class function TypeRetrieve<T>(const Value: string; const References: TArray<string>): T;
  end;

{ TMessageRoleHelper }

constructor TMessageRoleHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TMessageRole>(Value,
            ['system', 'user', 'assistant', 'tool']);
end;

function TMessageRoleHelper.ToString: string;
begin
  case Self of
    TMessageRole.system:
      Exit('system');
    TMessageRole.user:
      Exit('user');
    TMessageRole.assistant:
      Exit('assistant');
    TMessageRole.tool:
      Exit('tool');
  end;
end;

{ TFinishReasonHelper }

constructor TFinishReasonHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TFinishReason>(Value,
            ['stop', 'length', 'model_length', 'error', 'tool_calls']);
end;

function TFinishReasonHelper.ToString: string;
begin
  case Self of
    TFinishReason.stop:
      Exit('stop');
    TFinishReason.length:
      Exit('length');
    TFinishReason.model_length:
      Exit('model_length');
    TFinishReason.error:
      Exit('error');
    TFinishReason.tool_calls:
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

constructor TContentTypeHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TContentType>(Value,
            ['text', 'image_url', 'reference', 'document_url', 'input_audio', 'thinking']);
end;

function TContentTypeHelper.ToString: string;
begin
  case Self of
    TContentType.text:
      Exit('text');
    TContentType.image_url:
      Exit('image_url');
    TContentType.reference:
      Exit('reference');
    TContentType.document_url:
      Exit('document_url');
    TContentType.input_audio:
      Exit('input_audio');
    TContentType.thinking:
      Exit('thinking');
  end;
end;

{ TContentTypeInterceptor }

function TContentTypeInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TContentType>.ToString;
end;

procedure TContentTypeInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TContentType.Create(Arg)));
end;

{ TToolTypeHelper }

constructor TToolTypeHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TToolType>(Value,
            ['web_search_premium', 'web_search', 'image_generation',
             'function', 'document_library', 'code_interpreter']);
end;

function TToolTypeHelper.ToString: string;
begin
  case Self of
    TToolType.web_search_premium:
      Exit('web_search_premium');
    TToolType.web_search:
      Exit('web_search');
    TToolType.image_generation:
      Exit('image_generation');
    TToolType.function:
      Exit('function');
    TToolType.document_library:
      Exit('document_library');
    TToolType.code_interpreter:
      Exit('code_interpreter');
  end;
end;

{ TFineTuningJobStatusHelper }

constructor TFineTuningJobStatusHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TFineTuningJobStatus>(Value, [
    'queued', 'started', 'validating', 'validated', 'running', 'failed_validation', 'failed',
    'success', 'cancelled', 'cancellation_requested']);
end;

function TFineTuningJobStatusHelper.ToString: string;
begin
  case Self of
    TFineTuningJobStatus.Queued:
      Exit('QUEUED');
    TFineTuningJobStatus.Started:
      Exit('STARTED');
    TFineTuningJobStatus.Validating:
      Exit('VALIDATING');
    TFineTuningJobStatus.Validated:
      Exit('VALIDATED');
    TFineTuningJobStatus.Running:
      Exit('RUNNING');
    TFineTuningJobStatus.Failed_validation:
      Exit('FAILED_VALIDATION');
    TFineTuningJobStatus.Failed:
      Exit('FAILED');
    TFineTuningJobStatus.Success:
      Exit('SUCCESS');
    TFineTuningJobStatus.Cancelled:
      Exit('CANCELLED');
    TFineTuningJobStatus.CancellationRequested:
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

constructor TFineTuningDataObjectKindHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TFineTuningDataObjectKind>(Value,
            ['job']);
end;

function TFineTuningDataObjectKindHelper.ToString: string;
begin
  case Self of
    TFineTuningDataObjectKind.Job:
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

constructor TFineTuningIntegrationTypeHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TFineTuningIntegrationType>(Value,
            ['wandb']);
end;

function TFineTuningIntegrationTypeHelper.ToString: string;
begin
  case self of
    TFineTuningIntegrationType.Wandb:
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

constructor TFineTuningObjectKindHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TFineTuningObjectKind>(Value,
            ['list']);
end;

function TFineTuningObjectKindHelper.ToString: string;
begin
  case self of
    TFineTuningObjectKind.List:
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

constructor TCodestralFinishReasonHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TCodestralFinishReason>(Value,
            ['stop', 'length', 'model_length', 'error', 'tool_calls']);
end;

function TCodestralFinishReasonHelper.ToString: string;
begin
  case Self of
    TCodestralFinishReason.stop:
      Exit('stop');
    TCodestralFinishReason.length_limite:
      Exit('length');
    TCodestralFinishReason.model_length:
      Exit('model_length');
    TCodestralFinishReason.error:
      Exit('error');
    TCodestralFinishReason.tool_calls:
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

constructor TFilePurposeHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TFilePurpose>(Value,
            ['fine-tune', 'batch', 'ocr', 'audio', 'image_generation']);
end;

function TFilePurposeHelper.ToString: string;
begin
  case self of
    TFilePurpose.FineTune:
      Exit('fine-tune');
    TFilePurpose.batch:
      Exit('batch');
    TFilePurpose.ocr:
      Exit('ocr');
    TFilePurpose.audio:
      Exit('audio');
    TFilePurpose.image_generation:
      Exit('image_generation');
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

constructor TToolChoiceHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TToolChoice>(Value,
            ['auto', 'any', 'required', 'none']);
end;

function TToolChoiceHelper.ToString: string;
begin
  case Self of
    TToolChoice.auto:
      Exit('auto');
    TToolChoice.any:
      Exit('any');
    TToolChoice.required:
      Exit('required');
    TToolChoice.none:
      Exit('none');
  end;
end;

{ TSampleTypeHelper }

constructor TSampleTypeHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TSampleType>(Value,
            ['pretrain', 'instruct', 'batch_request', 'batch_result', 'batch_error',
             'audio_input', 'image_generation', 'ocr_input']);
end;

function TSampleTypeHelper.ToString: string;
begin
  case Self of
    TSampleType.pretrain:
      Exit('pretrain');
    TSampleType.instruct:
      Exit('instruct');
    TSampleType.batch_request:
      Exit('batch_request');
    TSampleType.batch_result:
      Exit('batch_result');
    TSampleType.batch_error:
      Exit('batch_error');
    TSampleType.audio_input:
      Exit('audio_input');
    TSampleType.image_generation:
      Exit('image_generation');
    TSampleType.ocr_input:
      Exit('ocr_input');
  end;
end;

{ TSourceTypeHelper }

constructor TSourceTypeHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TSourceType>(Value,
            ['upload', 'repository', 'mistral']);
end;

function TSourceTypeHelper.ToString: string;
begin
  case Self of
    TSourceType.upload:
      Exit('upload');
    TSourceType.repository:
      Exit('repository');
    TSourceType.mistral:
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

constructor TRepositoryTypeHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TRepositoryType>(Value,
            ['github']);
end;

function TRepositoryTypeHelper.ToString: string;
begin
  case Self of
    TRepositoryType.github:
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

constructor TBatchStatusHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TBatchStatus>(Value,
            ['queued', 'running', 'success', 'failed', 'timeout_exceeded',
             'cancellation_requested', 'cancelled']);
end;

function TBatchStatusHelper.ToString: string;
begin
  case Self of
    TBatchStatus.Queued:
      Exit('QUEUED');
    TBatchStatus.Running:
      Exit('RUNNING');
    TBatchStatus.Success:
      Exit('SUCCESS');
    TBatchStatus.Failed:
      Exit('FAILED');
    TBatchStatus.Timeout_exceeded:
      Exit('TIMEOUT_EXCEEDED');
    TBatchStatus.Cancellation_requested:
      Exit('CANCELLATION_REQUESTED');
    TBatchStatus.Cancelled:
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

constructor TEndPointTypeHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TEndPointType>(Value,
            ['/v1/chat/completions', '/v1/embeddings', '/v1/fim/completions', '/v1/moderations']);
end;

function TEndPointTypeHelper.ToString: string;
begin
  case Self of
    TEndPointType.ChatCompletion:
      Exit('/v1/chat/completions');
    TEndPointType.Embeddings:
      Exit('/v1/embeddings');
    TEndPointType.FimCompletions:
      Exit('/v1/fim/completions');
    TEndPointType.Moderation:
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

{ TEnumValueRecovery }

class function TEnumValueRecovery.TypeRetrieve<T>(const Value: string;
  const References: TArray<string>): T;
var
  pInfo: PTypeInfo;
begin
  pInfo := TypeInfo(T);
  if pInfo.Kind <> tkEnumeration then
    raise Exception.Create('TRecovery.TypeRetrieve<T>: T is not an enumerated type');

  var index := IndexStr(Value.ToLower, References);
  if index = -1 then
    raise Exception.CreateFmt('%s : Unable to retrieve enum value.', [Value]);

  Move(index, Result, SizeOf(Result));
end;

{ THandoffExecutionTypeHelper }

constructor THandoffExecutionTypeHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<THandoffExecutionType>(Value,
            ['client', 'server']);
end;

function THandoffExecutionTypeHelper.ToString: string;
begin
  case Self of
    THandoffExecutionType.client:
      Exit('client');
    THandoffExecutionType.server:
      Exit('server');
  end;
end;

{ TResponseFormatTypeHelper }

constructor TResponseFormatTypeHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TResponseFormatType>(Value,
            ['text', 'json_object', 'json_schema']);
end;

function TResponseFormatTypeHelper.ToString: string;
begin
  case Self of
    TResponseFormatType.text:
      Exit('text');
    TResponseFormatType.json_object:
      Exit('json_object');
    TResponseFormatType.json_schema:
      Exit('json_schema');
  end;
end;

{ TMessageRoleInterceptor }

function TMessageRoleInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TMessageRole>.ToString;
end;

procedure TMessageRoleInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TMessageRole.Create(Arg)));
end;

{ TContentChunkTypeHelper }

constructor TContentChunkTypeHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TContentChunkType>(Value,
            ['text', 'image_url', 'tool_file', 'document_url', 'tool_reference', 'thinking']);
end;

function TContentChunkTypeHelper.ToString: string;
begin
  case self of
    TContentChunkType.text:
      Exit('text');
    TContentChunkType.image_url:
      Exit('image_url');
    TContentChunkType.tool_file:
      Exit('tool_file');
    TContentChunkType.document_url:
      Exit('document_url');
    TContentChunkType.tool_reference:
      Exit('tool_reference');
    TContentChunkType.thinking:
      Exit('thinking');
  end;
end;

{ TContentChunkTypeInterceptor }

function TContentChunkTypeInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TContentChunkType>.ToString;
end;

procedure TContentChunkTypeInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TContentChunkType.Create(Arg)));
end;

{ TConversationToolHelper }

constructor TConversationToolHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TConversationTool>(Value,
            ['web_search', 'web_search_premium', 'code_interpreter',
             'image_generation', 'document_library']);
end;

function TConversationToolHelper.ToString: string;
begin
  case self of
    TConversationTool.web_search:
      Exit('web_search');
    TConversationTool.web_search_premium:
      Exit('web_search_premium');
    TConversationTool.code_interpreter:
      Exit('code_interpreter');
    TConversationTool.image_generation:
      Exit('image_generation');
    TConversationTool.document_library:
      Exit('document_library');
  end;
end;

{ TConversationToolInterceptor }

function TConversationToolInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TConversationTool>.ToString;
end;

procedure TConversationToolInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TConversationTool.Create(Arg)));
end;

{ TChunkEventHelper }

constructor TChunkEventHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TChunkEvent>(Value,
            ['conversation.response.started', 'conversation.response.done',
             'conversation.response.error', 'message.output.delta',
             'tool.execution.started', 'tool.execution.done',
             'agent.handoff.started', 'agent.handoff.done',
             'function.call.delta']);
end;

function TChunkEventHelper.ToString: string;
begin
  case self of
    TChunkEvent.conversation_response_started:
      Exit('conversation.response.started');
    TChunkEvent.conversation_response_done:
      Exit('conversation.response.done');
    TChunkEvent.conversation_response_error:
      Exit('conversation.response.error');
    TChunkEvent.message_output_delta:
      Exit('message.output.delta');
    TChunkEvent.tool_execution_started:
      Exit('tool.execution.started');
    TChunkEvent.tool_execution_done:
      Exit('tool.execution.done');
    TChunkEvent.agent_handoff_started:
      Exit('agent.handoff.started');
    TChunkEvent.agent_handoff_done:
      Exit('agent.handoff.done');
    TChunkEvent.function_call_delta:
      Exit('function.call.delta');
  end;
end;

{ TChunkEventInterceptor }

function TChunkEventInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TChunkEvent>.ToString;
end;

procedure TChunkEventInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TChunkEvent.Create(Arg)));
end;

{ TReasoningInstructionHelper }

function TReasoningInstructionHelper.ToString: string;
begin
  case Self of
    Custom:
      Exit('custom');
    Default:
      Exit(ReasoningEnglishInstructions);
  end;
end;

{ TLevelTypeHelper }

constructor TLevelTypeHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TLevelType>(Value,
            ['viewer', 'editor']);
end;

function TLevelTypeHelper.ToString: string;
begin
  case Self of
    TLevelType.viewer:
      Exit('Viewer');
    TLevelType.editor:
      Exit('Editor');
  end;
end;

{ TShareWithTypeHelper }

constructor TShareWithTypeHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TShareWithType>(Value,
            ['user', 'workspace', 'org']);
end;

function TShareWithTypeHelper.ToString: string;
begin
  case Self of
    TShareWithType.user:
      Exit('User');
    TShareWithType.workspace:
      Exit('Workspace');
    TShareWithType.org:
      Exit('Org');
  end;
end;

{ TShareWithTypeInterceptor }

function TShareWithTypeInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TShareWithType>.ToString;
end;

procedure TShareWithTypeInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TShareWithType.Create(Arg)));
end;

{ TConversatonEventHelper }

constructor TConversatonEventHelper.Create(const Value: string);
begin
  Self := TEnumValueRecovery.TypeRetrieve<TConversatonEvent>(Value,
            ['message.input', 'message.output', 'tool.execution', 'function.call', 'agent.handoff']);
end;

function TConversatonEventHelper.ToString: string;
begin
  case Self of
    TConversatonEvent.message_input:
      Exit('message.input');
    TConversatonEvent.message_output:
      Exit('message.output');
    TConversatonEvent.tool_execution:
      Exit('tool.execution');
    TConversatonEvent.function_call:
      Exit('function.call');
    TConversatonEvent.agent_handoff:
      Exit('agent.handoff');
  end;
end;

{ TConversatonEventInterceptor }

function TConversatonEventInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TConversatonEvent>.ToString;
end;

procedure TConversatonEventInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TConversatonEvent.Create(Arg)));
end;

end.

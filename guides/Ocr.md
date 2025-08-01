# OCR API (v1/ocr)

- [Basic OCR](#basic-ocr)
   - [Example 1](#example-1)
- [Annotations](#annotations)
- [Document QnA](#document-qna)

<br>

___

## Basic OCR

The Mistral Document AI API includes a high-performance **OCR (Optical Character Recognition)*** processor, powered by the `mistral-ocr-latest` model. It enables accurate extraction of text and document structure from a wide variety of digital formats.

### Key Features
- **Text recognition with structural preservation** <br> 
Extracts content while **retaining the logical structure** of the document :

   - Headers, paragraphs, lists, and tables

   - Complex layouts like multi-column formats and mixed content (text + visuals)

- **Markdown output format** <br>
Results are returned in **Markdown**, making it easy to:

   - Preview content

   - Parse programmatically

   - Render in UI components or markdown-based workflows

- **Multi-format support**

   - Images (`image_url`): `.png`, `.jpeg/.jpg`, `.avif`, and more

   - Documents (`document_url`): `.pdf`, `.pptx`, `.docx`, and others

- **Scalable & accurate**

   - Optimized for **large-scale processing** with **high accuracy**, even on complex layouts.

- **Rich output**

   - The processor returns:

        - **Recognized text**

        - **Bounding boxes for images**

        - **Metadata** about the document’s structure

- Enables **fine-grained, programmable access** to the document’s content.

### In Summary

Mistral's `OCR engine` doesn’t just read text—it **understands and preserves document layout and semantics**, offering a **robust foundation** for downstream use cases like data extraction, content analysis, summarization, or advanced annotation.

<br>

### Example 1

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Ocr.AsyncAwaitCreate(
    procedure (Params: TOcrParams)
    begin
      Params.Model('mistral-ocr-latest');
      Params.Document(
        TOcrDocumentParams.NewDocument
          .DocumentUrl('..\..\sample\File_Search_file.pdf'));
      Params.IncludeImageBase64;
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  Promise
    .&Then<TOcr>(
      function (Value: TOcr): TOcr
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.Ocr.AsyncCreate(
//    procedure (Params: TOcrParams)
//    begin
//      Params.Model('mistral-ocr-latest');
//      Params.Document(
//        TOcrDocumentParams.NewDocument
//          .DocumentUrl('..\..\sample\File_Search_file.pdf'));
//      Params.IncludeImageBase64;
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsyncOcr
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.Ocr.Create(
//    procedure (Params: TOcrParams)
//    begin
//      Params.Model('mistral-ocr-latest');
//      Params.Document(
//        TOcrDocumentParams.NewDocument
//          .DocumentUrl('..\..\sample\File_Search_file.pdf'));
//      Params.IncludeImageBase64;
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end);
//  try
//    TutorialHub.JSONResponse := Value.JSONResponse;
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Annotations

In addition to basic OCR, Mistral Document AI offers advanced annotation capabilities that enable structured information extraction in a custom JSON format defined by the user. This system supports two main types of annotations:

### Types of Annotations

1. `bbox_annotation`
Used to annotate specific visual elements within a document based on bounding boxes (e.g., figures, tables, charts, or signatures). This allows users to extract visual components according to their requirements and convert them into structured outputs. It also supports tasks like describing or captioning figures based on their content and layout.

2. `document_annotation`
Applies structured annotations at the document level. The user provides a target JSON structure, and the model extracts and maps relevant information from the entire document to match this structure. This is useful for turning semi-structured or unstructured content into machine-usable data.

### Key Capabilities
- Extraction of structured data from documents using a predefined schema.

- Automated data annotation and labeling, reducing manual processing effort.

- Minimization of input errors through consistent parsing.

- Scalability to handle high document volumes in enterprise contexts.

- Support for visual, textual, and semantic data processing.

### Common Use Cases

- Automated extraction of fields from forms or scanned documents.

- Document classification with metadata tagging (e.g., invoices, receipts, contracts).

- Conversion of charts and figures into structured data (e.g., tables).

- Extraction of key information from financial documents: 

    - From receipts: merchant name, date, total amount.

    - From invoices: supplier name, line items, tax values.

    - From contracts: key clauses, deadlines, and legal terms.

- Detection and annotation of fine print, footnotes, watermarks, and signatures.

### Annotations Explanation Graph

An optional component that provides a visual and logical trace of how annotations were derived. This can help with model validation, transparency, and debugging.

### Summary

Mistral Document AI is designed not just to recognize text, but to interpret and structure document content according to business-specific needs. Its annotation features make it especially well-suited for high-volume, high-accuracy use cases in fields such as finance, legal, compliance, and enterprise document management.

If you’d like, I can provide an example JSON schema or walk through a specific integration scenario.

<br>

___

## Document QnA




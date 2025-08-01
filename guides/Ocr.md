# OCR API (v1/ocr)

- [Basic OCR](#basic-ocr)
   - [Example 1](#example-1)
- [Annotations](#annotations)
   - [BBox Annotation](#bbox-annotation)
   - [Document Annotation](#document-annotation)
   - [BBoxes Annotation and Document Annotation](#bboxes-annotation-and-document-annotation)
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

### BBox Annotation

- [Example 2](#example-2)
- [Example 3](#example-3)

#### Example 2

The request is structured to ensure that the response adheres to the specified custom JSON schema. The schema defines the structure of a bbox_annotation object with image_type, short_description and summary properties.

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  Client.HttpClient.ResponseTimeout := 1200000;

  var Annotations :=
    '{' +
    '  "schema": {' +
    '      "properties": {' +
    '          "document_type": {"title": "Document_Type", "type": "string"},' +
    '          "short_description": {"title": "Short_Description", "type": "string"},' +
    '          "summary": {"title": "Summary", "type": "string"}' +
    '      },' +
    '      "required": ["document_type", "short_description", "summary"],' +
    '      "title": "BBOXAnnotation",' +
    '      "type": "object",' +
    '      "additionalProperties": false' +
    '  },' +
    '  "name": "document_annotation",' +
    '  "strict": true' +
    '}';

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Ocr.AsyncAwaitCreate(
    procedure (Params: TOcrParams)
    begin
      Params.Model('mistral-ocr-latest');
      Params.Document(
        TOcrDocumentParams.NewDocument
          .DocumentUrl('https://arxiv.org/pdf/2410.07073'));
      Params.BboxAnnotationFormat(TResponseFormatParams.Json_Schema(Annotations));
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
//          .DocumentUrl('https://arxiv.org/pdf/2410.07073'));
//      Params.BboxAnnotationFormat(TResponseFormatParams.Json_Schema(Annotations));
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
//          .DocumentUrl('https://arxiv.org/pdf/2410.07073'));
//      Params.BboxAnnotationFormat(TResponseFormatParams.Json_Schema(Annotations));
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

#### Example 3

You can also add a description key in your properties object. The description will be used as detailed information and instructions during the annotation; for example:

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  Client.HttpClient.ResponseTimeout := 1200000;

  var Annotations :=
    '{' +
    '        "schema": {' +
    '            "properties": {' +
    '                "document_type": {"title": "Document_Type", "description": "The type of the image.", "type": "string"},' +
    '                "short_description": {"title": "Short_Description", "description": "A description in English describing the image.", "type": "string"},' +
    '                "summary": {"title": "Summary", "description": "Summarize the image.", "type": "string"}' +
    '            },' +
    '            "required": ["document_type", "short_description", "summary"],' +
    '            "title": "BBOXAnnotation",' +
    '            "type": "object",' +
    '            "additionalProperties": false' +
    '        },' +
    '        "name": "document_annotation",' +
    '        "strict": true' +
    '    }';

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Ocr.AsyncAwaitCreate(
    procedure (Params: TOcrParams)
    begin
      Params.Model('mistral-ocr-latest');
      Params.Document(
        TOcrDocumentParams.NewDocument
          .DocumentUrl('https://arxiv.org/pdf/2410.07073'));
      Params.BboxAnnotationFormat(TResponseFormatParams.Json_Schema(Annotations));
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
//          .DocumentUrl('https://arxiv.org/pdf/2410.07073'));
//      Params.BboxAnnotationFormat(TResponseFormatParams.Json_Schema(Annotations));
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
//          .DocumentUrl('https://arxiv.org/pdf/2410.07073'));
//      Params.BboxAnnotationFormat(TResponseFormatParams.Json_Schema(Annotations));
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

### Document Annotation

The request is structured to ensure that the response adheres to the specified custom JSON schema. The schema defines the structure of a document_annotation object with with language, chapter_titles and urls properties.

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  Client.HttpClient.ResponseTimeout := 1200000;

  var Annotations :=
    '{' +
    '        "schema": {' +
    '            "properties": {' +
    '                "language": {"title": "Language", "type": "string"},' +
    '                "chapter_titles": {"title": "Chapter_Titles", "type": "string"},' +
    '                "urls": {"title": "urls", "type": "string"}' +
    '            },' +
    '            "required": ["language", "chapter_titles", "urls"],' +
    '            "title": "DocumentAnnotation",' +
    '            "type": "object",' +
    '            "additionalProperties": false' +
    '        },' +
    '        "name": "document_annotation",' +
    '        "strict": true' +
    '    }';

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Ocr.AsyncAwaitCreate(
    procedure (Params: TOcrParams)
    begin
      Params.Model('mistral-ocr-latest');
      Params.Document(
        TOcrDocumentParams.NewDocument
          .DocumentUrl('https://arxiv.org/pdf/2410.07073'));
      Params.Pages([0, 1, 2, 3, 4, 5, 6, 7]);
      Params.DocumentAnnotationFormat(TResponseFormatParams.Json_Schema(Annotations));
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
//          .DocumentUrl('https://arxiv.org/pdf/2410.07073'));
//      Params.Pages([0, 1, 2, 3, 4, 5, 6, 7]);
//      Params.BboxAnnotationFormat(TResponseFormatParams.Json_Schema(Annotations));
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
//          .DocumentUrl('https://arxiv.org/pdf/2410.07073'));
//      Params.Pages([0, 1, 2, 3, 4, 5, 6, 7]);
//      Params.BboxAnnotationFormat(TResponseFormatParams.Json_Schema(Annotations));
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
___

### BBoxes Annotation and Document Annotation

- [Example 4](#example-4)
- [Example 5](#example-5)

<br>

___

#### Example 4

The request is structured to ensure that the response adheres to the specified custom JSON schema. The schema defines the structure of a bbox_annotation object with image_type, short_description and summary properties and a document_annotation object with with language, chapter_titles and urls properties.

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  Client.HttpClient.ResponseTimeout := 1200000;

  var BoxAnnotations :=
    '{' +
    '        "schema": {' +
    '            "properties": {' +
    '                "document_type": {"title": "Document_Type", "description": "The type of the image.", "type": "string"},' +
    '                "short_description": {"title": "Short_Description", "description": "A description in English describing the image.", "type": "string"},' +
    '                "summary": {"title": "Summary", "description": "Summarize the image.", "type": "string"}' +
    '            },' +
    '            "required": ["document_type", "short_description", "summary"],' +
    '            "title": "BBOXAnnotation",' +
    '            "type": "object",' +
    '            "additionalProperties": false' +
    '        },' +
    '        "name": "document_annotation",' +
    '        "strict": true' +
    '    }';

  var DocumentAnnotations :=
    '{' +
    '        "schema": {' +
    '            "properties": {' +
    '                "language": {"title": "Language", "type": "string"},' +
    '                "chapter_titles": {"title": "Chapter_Titles", "type": "string"},' +
    '                "urls": {"title": "urls", "type": "string"}' +
    '            },' +
    '            "required": ["language", "chapter_titles", "urls"],' +
    '            "title": "DocumentAnnotation",' +
    '            "type": "object",' +
    '            "additionalProperties": false' +
    '        },' +
    '        "name": "document_annotation",' +
    '        "strict": true' +
    '    }';

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Ocr.AsyncAwaitCreate(
    procedure (Params: TOcrParams)
    begin
      Params.Model('mistral-ocr-latest');
      Params.Document(
        TOcrDocumentParams.NewDocument
          .DocumentUrl('https://arxiv.org/pdf/2410.07073'));
      Params.Pages([0, 1, 2, 3, 4, 5, 6, 7]);
      Params.BBoxAnnotationFormat(TResponseFormatParams.Json_Schema(BoxAnnotations));
      Params.DocumentAnnotationFormat(TResponseFormatParams.Json_Schema(DocumentAnnotations));
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
//          .DocumentUrl('https://arxiv.org/pdf/2410.07073'));
//      Params.Pages([0, 1, 2, 3, 4, 5, 6, 7]);
//      Params.BboxAnnotationFormat(TResponseFormatParams.Json_Schema(Annotations));
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
//          .DocumentUrl('https://arxiv.org/pdf/2410.07073'));
//      Params.Pages([0, 1, 2, 3, 4, 5, 6, 7]);
//      Params.BboxAnnotationFormat(TResponseFormatParams.Json_Schema(Annotations));
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

#### Example 5

You can also add a description key in you properties object. The description will be used as detailed information and instructions during the annotation; for example:

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  Client.HttpClient.ResponseTimeout := 1200000;

  var BoxAnnotations :=
    '{' +
    '        "schema": {' +
    '            "properties": {' +
    '                "document_type": {"title": "Document_Type", "description": "The type of the image.", "type": "string"},' +
    '                "short_description": {"title": "Short_Description", "description": "A description in English describing the image.", "type": "string"},' +
    '                "summary": {"title": "Summary", "description": "Summarize the image.", "type": "string"}' +
    '            },' +
    '            "required": ["document_type", "short_description", "summary"],' +
    '            "title": "BBOXAnnotation",' +
    '            "type": "object",' +
    '            "additionalProperties": false' +
    '        },' +
    '        "name": "document_annotation",' +
    '        "strict": true' +
    '    }';

  var DocumentAnnotations :=
    '{' +
    '        "schema": {' +
    '            "properties": {' +
    '                "language": {"title": "Language", "type": "string"},' +
    '                "chapter_titles": {"title": "Chapter_Titles", "type": "string"},' +
    '                "urls": {"title": "urls", "type": "string"}' +
    '            },' +
    '            "required": ["language", "chapter_titles", "urls"],' +
    '            "title": "DocumentAnnotation",' +
    '            "type": "object",' +
    '            "additionalProperties": false' +
    '        },' +
    '        "name": "document_annotation",' +
    '        "strict": true' +
    '    }';

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Ocr.AsyncAwaitCreate(
    procedure (Params: TOcrParams)
    begin
      Params.Model('mistral-ocr-latest');
      Params.Document(
        TOcrDocumentParams.NewDocument
          .DocumentUrl('https://arxiv.org/pdf/2410.07073'));
      Params.BBoxAnnotationFormat(TResponseFormatParams.Json_Schema(BoxAnnotations));
      Params.DocumentAnnotationFormat(TResponseFormatParams.Json_Schema(DocumentAnnotations));
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
//          .DocumentUrl('https://arxiv.org/pdf/2410.07073'));
//      Params.Pages([0, 1, 2, 3, 4, 5, 6, 7]);
//      Params.BboxAnnotationFormat(TResponseFormatParams.Json_Schema(Annotations));
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
//          .DocumentUrl('https://arxiv.org/pdf/2410.07073'));
//      Params.Pages([0, 1, 2, 3, 4, 5, 6, 7]);
//      Params.BboxAnnotationFormat(TResponseFormatParams.Json_Schema(Annotations));
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

## Document QnA

The **Document QnA** feature combines Optical Character Recognition (OCR) with the power of a **large language model (LLM)** to enable **natural language interaction with document content**.

In practical terms, this means you can **ask questions directly about a document**—as if you were speaking to a subject matter expert—and receive **precise, context-aware answers** derived from the actual content.

<br>

### How It Works

The workflow consists of two main steps:

1. **Document Processing**

OCR analyzes the document to extract raw text, structural elements (headings, sections, tables, etc.), and formatting. The result is a machine-readable version of the document, ready for semantic analysis.

2. **Language Model Understanding**

The extracted content is passed to a** large language model**, which interprets the document in full. Users can then **ask natural language questions**, and the model provides answers that take into account the **full context and internal relationships** of the document.

<br>

### Key Capabilities

- **Natural language question answering** about specific document content

- **Targeted information extraction** without manual review

- **Automatic summarization** of key points

- **In-depth document analysis** (inference, reformulation, comparisons)

- **Multi-document querying** and content comparison

- **Context-aware responses** based on the document as a whole

<br>

### Common Use Cases

- **Analyzing research papers or technical reports** <br>
Example: extract experimental results or understand the methodology section.

- **Extracting structured information from business documents** <br>
Example: locate a contract number, payment terms, or approval conditions.

- **Processing legal and regulatory documents** <br>
Example: identify obligations, penalty clauses, or termination terms in a contract.

- **Building document-based Q&A applications** <br>
Example: create internal chatbots to assist with policy documents or compliance reports.

- **Automating document-driven workflows** <br>
Example: pre-validate annex content or auto-fill forms from PDF data.

<br>

### Summary

**Document QnA** turns static files like PDFs or scans into interactive information sources, accessible via natural language. It's especially well-suited for use cases that require fast, reliable, and structured access to complex content—such as legal, research, finance, and compliance workflows.

The examples below show how to interact with a PDF document using natural language:

### Upload the Image File

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  //Synchronous example
  var Upload := Client.&File.Upload(
    procedure (Params: TUploadParams)
    begin
      Params.Purpose(TFilePurpose.ocr);
      Params.&File('..\..\sample\File_Search_file.pdf');
    end);
  try
    Display(Memo1, Upload);
    //get id_file
  finally
    Upload.Free;
  end;
```

### Get the Signed URL

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  //Synchronous example
  var Value := Client.&File.GetSignedUrl(id_file,
    procedure (Params: TSignedUrlParams)
    begin
      Params.Expiry(24);  //expires after 24 hours
    end);
  try
    Display(TutorialHub, Value.Url);
  //get signed_url
  finally
    Value.Free;
  end;
```

### Chat Completion

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  var SignedUrl := 'signed_url';

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Chat.AsyncAwaitCreate(
    procedure (Params: TChatParams)
    begin
      Params.Model('mistral-small-latest');
      Params.Messages([
        Payload.User(
          'what is the last sentence in the document',
          TDocumentUrlParams.New.DocumentUrl(SignedUrl)
        )
      ]);
      Params.DocumentImageLimit(8);
      Params.DocumentPageLimit(64);
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  promise
    .&Then<string>(
      function (Value: TChat): string
      begin
        for var Item in Value.Choices do
          Result := Result + Item.Message.Content[0].Text;
        Display(TutorialHub, Value);
        ShowMessage(Result);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
```
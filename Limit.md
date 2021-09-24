/// Functionality to limit HTTP requests based on header and body information.
[<AutoOpen>]
module Limit =
  type private Limit = unit

  /// Limits to only requests with one of the specified `Accept` headers,
  /// returning `415 UnsupportedMediaType` when the request header doesn't exists in the set of specified accept headers or no `Accept` header can be found.
  let mustAcceptOneOf (contentTypes : string seq) =
    fun next (ctx : HttpContext) ->
      let logger = ctx.GetLogger<Limit> ()
      let accept = ctx.Request.GetTypedHeaders().Accept
      match Option.ofObj accept with
      | Some a when Seq.exists (fun ct -> a |> Seq.exists (fun (x : MediaTypeHeaderValue) -> x.IsSubsetOf (MediaTypeHeaderValue(StringSegment(ct))))) contentTypes -> next ctx
      | Some _ -> 
          logger.LogError("Could not process request because the request does not accept the expected content as response (Accept: {Accept})", accept);
          RequestErrors.unsupportedMediaType (text "request header 'Accept' doesn't have expected value") earlyReturn ctx
      | None -> 
          logger.LogError("Could not process request because the request does not have an 'Accept' header")
          RequestErrors.unsupportedMediaType (text "expected to have request header 'Accept'") earlyReturn ctx
  
  /// Limits to only requests with a specific `Accept` header,
  /// returning `415 UnsupportedMediaType` when the request header doesn't match the specified type or no `Accept` header can be found.
  let mustAccept contentType = mustAcceptOneOf [ contentType ]

  /// Limits to only requests with one of the specified `Content-Type` headers,
  /// returning `415 UnsupportedMediaType` when the request header doesn't exists in the set of specified types or no `Content-Type` header can be found.
  let haveOneOfContentTypes contentTypes =
    fun next (ctx : HttpContext) ->
      let logger = ctx.GetLogger<Limit> ()
      let contentType = ctx.Request.ContentType
      match Option.ofObj contentType with
      | Some t when Seq.contains t contentTypes -> next ctx
      | Some _ -> 
          logger.LogError("Could not process request because the request is not the correct content (Content-Type: {ContentType})", contentType)
          RequestErrors.unsupportedMediaType (text "request header 'Content-Type' doesn't had expected value") earlyReturn ctx
      | None -> 
          logger.LogError("Could not process request because the request does not have a 'Content-Type' header")
          RequestErrors.unsupportedMediaType (text "expected to have request header 'Content-Type'") earlyReturn ctx

  /// Limits to only requests with a specific `Content-Type` header, 
  /// returning `415 UnsupportedMediaType` when the request header value doesn't match the specified type or no `Content-Type` header can be found.
  let haveContentType contentType = haveOneOfContentTypes [ contentType ]

  /// Limits request `Content-Length` header to a specified length, 
  /// returning `406 NotAcceptable` when no such header is present or the value exceeds the maximum specified length.
  let lessOrEqualContentLength l =
    fun next (ctx : HttpContext) ->
      let logger = ctx.GetLogger<Limit> ()
      let contentLength = ctx.Request.GetTypedHeaders().ContentLength
      match Option.ofNullable (contentLength) with
      | Some v when v <= l ->  next ctx
      | Some _ -> 
          logger.LogError("Could not process request because the request body is too large (Content-Length: {Content-Length})", contentLength)
          RequestErrors.notAcceptable (text "request header 'Content-Length' is too large") earlyReturn ctx
      | None -> 
          logger.LogError("Could not process request because the request does not have a 'Content-Length' header")
          RequestErrors.notAcceptable (text "request doesn't contain 'Content-Length' header") earlyReturn ctx

  /// Limits the request body size to a specified length, returing `413 Payload Too Large` if the body size exceeds the specified maximum.
  let maxBodySize l =
    fun next (ctx : HttpContext) ->
      ctx.Features.Get<IHttpMaxRequestBodySizeFeature>().MaxRequestBodySize <- System.Nullable<_> l
      try next ctx
      with :? BadHttpRequestException as ex when ex.StatusCode = 413 ->
        let logger = ctx.GetLogger<Limit> ()
        logger.LogError("Could not process request because request body is too large (Max length: {MaxLength})", l)
        (clearResponse >=> setStatusCode ex.StatusCode >=> text "request body too large") earlyReturn ctx

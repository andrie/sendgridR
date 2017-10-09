


#' Constructs list with email and name of person
#'
#' Used to construct the email payload. See [make_email_payload] and [sendgrid_send]
#'
#' @param email email address
#' @param name name
#'
#' @return list of length one
#' @export
#' @family email functions
email_person <- function(email, name){
  z <- list(
    email = unbox(email),
    name= unbox(name)
  )
  class(z) <- c("email_person", "list")
  z
}


#' Constructs data frame with email and names of people
#'
#' Used to construct the email payload. See [make_email_payload] and [sendgrid_send]
#' @inherit email_person
#' @export
email_group <- function(email, name){
  z <- data.frame(
    email = email,
    name = name,
    stringsAsFactors = FALSE
  )
  class(z) <- c("email_group", "data.frame")
  z
}


#' Constructs email payload from emails, subject and body
#'
#' Once the payload is constructed, you can use [sendgrid_send] to send the message
#'
#' @param to [email_group]
#' @param subject character vector with subject of message
#' @param body character vector with body of message
#' @param from [email_person]
#' @param reply_to [email_person]
#'
#' @return list with payload in the format required by [sendgrid_send]
#' @export
#' @family email functions
make_email_payload <- function(to, subject, body, from, reply_to = from){
  if(inherits(to, "email_person")){
    to <- as.data.frame(to, stringsAsFactors = FALSE)
  }
  list(
    personalizations = I(data.frame(
      to = I(list(to)
      ),
      subject = unbox(subject),
      stringsAsFactors = FALSE
    )),
    from = from,
    reply_to = reply_to,
    subject = unbox(subject),
    content = data.frame(
      type = "text/html",
      value = body,
      stringsAsFactors = FALSE)
  )
}


#' Sends mail using sendgrid API v3
#'
#' @param payload email payload, see [make_email_payload]
#' @param api_key character string, obtained as API key in sendgrid app
#'
#' @return Called for its side effect of sending the payload as an email using the sendgrid API. Returns NULL if successful
#'
#' @references https://sendgrid.api-docs.io/v3.0/mail-send
#' @export
#' @family email functions
sendgrid_send <- function(payload, api_key){
  h <- handle("api.sendgrid.com")
  headers <- add_headers(
    authorization = sprintf("Bearer %s", api_key),
    `content-type` = "application/json"
  )
  r <- POST("https://api.sendgrid.com/v3/mail/send",
            headers, body = toJSON(payload), handle = h)
  # browser()
  if(!is.null(content(r))){
    msg <- content(r)
    warning(paste(msg, sep = "\n", collapse = "\n"))
  }
  r

}

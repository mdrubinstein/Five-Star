# Read and process yaml
yaml_process <- function(file) {
  file %>%
    yaml::yaml.load_file() %>%
    str_split(' ') %>%
    unlist() 
}

# Write file to yaml output
writeYml <- function(list, filename){
  list <- yaml::as.yaml(list)
  con <- file(filename, 'w')
  writeLines(list, con)
  close(con)
}

# Capitalize first word (taken from stackoverflow)
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# Create time lag for inputs (taken from stackoverflow)
debounce <- function(expr, millis, env = parent.frame(), quoted = FALSE,
                     domain = getDefaultReactiveDomain()) {
  
  force(millis)
  
  f <- exprToFunction(expr, env, quoted)
  label <- sprintf("debounce(%s)", paste(deparse(body(f)), collapse = "\n"))
  
  v <- reactiveValues(
    trigger = NULL,
    when = NULL # the deadline for the timer to fire; NULL if not scheduled
  )  
  
  # Responsible for tracking when f() changes.
  observeEvent(f(), {
    # The value changed. Start or reset the timer.
    v$when <- Sys.time() + millis/1000
  }, ignoreNULL = FALSE)
  
  # This observer is the timer. It rests until v$when elapses, then touches
  # v$trigger.
  observe({
    if (is.null(v$when))
      return()
    
    now <- Sys.time()
    if (now >= v$when) {
      v$trigger <- runif(1)
      v$when <- NULL
    } else {
      invalidateLater((v$when - now) * 1000, domain)
    }
  })
  
  # This is the actual reactive that is returned to the user. It returns the
  # value of f(), but only invalidates/updates when v$trigger is touched.
  eventReactive(v$trigger, {
    f()
  }, ignoreNULL = FALSE)
}
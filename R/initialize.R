
initialize <- function(daps,
                       n, 
                       init_table = NULL, 
                       static = NULL, 
                       longitudinal = NULL) {
  
  daps$simulation_protocol <-
    structure(
      list(
        n = n,
        data = list(static = static, longitudinal = longitudinal),
        protocol = init_table
      ),
      class = "daps_simulation_protocol"
    )
  
  daps
}


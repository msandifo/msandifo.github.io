my_works <-orcid_works(orcid = "0000-0002-9757-745X")
my_works[[1]]$works$title.title.value |> unique() -> titles
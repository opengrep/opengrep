# params()[...] should match hash access on method calls
#ERROR: match
params[:id]

#ERROR: match
sink(params[:to])

#ERROR: match
params()[:name]

# Local variable should NOT match params()[...]
params = { id: 1 }
x = params[:id]

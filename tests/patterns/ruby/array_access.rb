# params[...] should match hash access on bare identifiers (method calls)
#ERROR: match
params[:id]

#ERROR: match
sink(params[:to])

# Also match explicit call form
#ERROR: match
params()[:name]

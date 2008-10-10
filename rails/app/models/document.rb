class Document < ActiveResource::Base
  self.site = "http://localhost:8000/"
  self.format = :json
end

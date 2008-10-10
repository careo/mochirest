class DocumentsController < ApplicationController
  # GET /documents
  # GET /documents.xml
  def index
    @documents = Document.find(:all)

    respond_to do |format|
      format.html # index.html.erb
      format.json  { render :json => @documents }
    end
  end

  # GET /documents/1
  # GET /documents/1.json
  def show
    @document = Document.find(params[:id])

    respond_to do |format|
      format.html # show.html.erb
      format.json  { render :json => @document }
    end
  end

  # GET /documents/new
  # GET /documents/new.json
  def new
    @document = Document.new(:id => nil, :name => nil, :body => nil)
    # stupid activeresource throws an error unless you assign something ^
    # to the attributes = LAME = FAIL!

    respond_to do |format|
      format.html # new.html.erb
      format.json  { render :json => @document }
    end
  end

  # GET /documents/1/edit
  def edit
    @document = Document.find(params[:id])
  end

  # POST /documents
  # POST /documents.json
  def create
    @document = Document.new(params[:document])

    respond_to do |format|
      if @document.save
        flash[:notice] = 'Document was successfully created.'
        format.html { redirect_to(@document) }
        format.json  { render :json => @document, :status => :created, :location => @document }
      else
        format.html { render :action => "new" }
        format.json  { render :json => @document.errors, :status => :unprocessable_entity }
      end
    end
  end

  # PUT /documents/1
  # PUT /documents/1.json
  def update
    @document = Document.find(params[:id])

    respond_to do |format|
      [ :name, :body ].each do |attr|
        @document.send("#{attr}=", params[:document][attr])
      end
      if @document.save
        flash[:notice] = 'Document was successfully updated.'
        format.html { redirect_to(@document) }
        format.json  { head :ok }
      else
        format.html { render :action => "edit" }
        format.json  { render :json => @document.errors, :status => :unprocessable_entity }
      end
    end
  end

  # DELETE /documents/1
  # DELETE /documents/1.json
  def destroy
    @document = Document.find(params[:id])
    @document.destroy

    respond_to do |format|
      format.html { redirect_to(documents_url) }
      format.json { head :ok }
    end
  end
end

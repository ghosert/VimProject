/**
 * © 2012 StubHub, Inc. All rights reserved.
 */
package com.stubhub.ui.business.process.impl;

import java.io.InputStream;
import java.security.AccessControlException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import javax.xml.bind.JAXBException;

import nu.xom.Document;
import nu.xom.Element;

import org.apache.log4j.LogSF;
import org.apache.log4j.Logger;
import org.apache.tapestry5.upload.services.UploadedFile;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import com.stubhub.common.ErrorConstants;
import com.stubhub.common.UserGuid;
import com.stubhub.common.business.entity.StubCountries;
import com.stubhub.common.business.entity.StubHubFile;
import com.stubhub.common.business.enums.PDFReasonCode;
import com.stubhub.common.business.facade.CodesFacade;
import com.stubhub.common.business.facade.EmailFacade;
import com.stubhub.common.business.util.DomainUtil;
import com.stubhub.common.business.util.NewDateUtils;
import com.stubhub.common.config.RequestContext;
import com.stubhub.common.config.StubhubCredentials;
import com.stubhub.common.exceptions.base.Argument;
import com.stubhub.common.exceptions.base.InvalidArgumentException;
import com.stubhub.common.exceptions.base.MissingRequiredArgumentException;
import com.stubhub.common.exceptions.base.StubHubBizException;
import com.stubhub.common.exceptions.base.StubHubFileException;
import com.stubhub.common.util.CommonUtil;
import com.stubhub.common.util.Log4jDBUtil;
import com.stubhub.common.util.StringUtils;
import com.stubhub.common.util.StubHubProperties;
import com.stubhub.content.business.facade.ContentFacade;
import com.stubhub.content.util.BaseContextDocumentUtil;
import com.stubhub.content.util.ContentDocumentUtil;
import com.stubhub.fulfillment.business.facade.TicketFileFacade;
import com.stubhub.fulfillment.business.facade.util.PdfParserHelper;
import com.stubhub.inventory.business.entity.Ticket;
import com.stubhub.inventory.business.sfr.facade.SFRListingProcessFacade;
import com.stubhub.resource.business.entity.UploadFileInfo;
import com.stubhub.ui.business.process.DeliveryProcess;
import com.stubhub.ui.model.Content;
import com.stubhub.ui.model.ContentContext;
import com.stubhub.ui.model.Listing;
import com.stubhub.ui.model.upload.TicketFile;
import com.stubhub.ui.model.upload.TicketSeat;
import com.stubhub.ui.model.upload.enums.PDFSplitterSelectionType;
import com.stubhub.ui.model.upload.enums.SeatType;
import com.stubhub.ui.model.upload.enums.SplitsSelectionType;
import com.stubhub.ui.services.RequestUserContext;
import com.stubhub.user.business.entity.SellOptions;
import com.stubhub.user.business.entity.UserContact;
import com.stubhub.user.business.facade.UserContactsFacade;
import com.stubhub.user.business.facade.UserFacade;
import com.stubhub.user.business.facade.UserSessionFacade;

/**
 * 
 * Process for non-split PDF sell flow 
 *
 */
@Component("deliveryProcess")
public class DeliveryProcessImpl extends CommonProcessImpl implements DeliveryProcess {
	private static Logger log = Logger.getLogger(DeliveryProcessImpl.class);
	
	private TicketFileFacade ticketFileFacade;
	private SFRListingProcessFacade sfrListingProcessFacade;	
	private ContentFacade contentFacade;
	private UserContactsFacade userContactsFacade;
	private UserSessionFacade userSessionFacade;
	private CodesFacade codesFacade;
	// send email for the listing deleted for fraudulent file
	private EmailFacade emailFacade;
	private UserFacade userFacadeBiz;
	
	private PdfParserHelper pdfParserHelper;
	
	@Autowired
	public void setTicketFileFacade(TicketFileFacade ticketFileFacade) {
		this.ticketFileFacade = ticketFileFacade;
	}

	@Autowired
	public void setSfrListingProcessFacade(SFRListingProcessFacade sfrListingProcessFacade) {
		this.sfrListingProcessFacade = sfrListingProcessFacade;
	}

	@Autowired
	public void setContentFacade(ContentFacade contentFacade) {
		this.contentFacade = contentFacade;
	}

	@Autowired
	public void setUserContactsFacade(@Qualifier("userContactsFacadeBiz") UserContactsFacade userContactsFacade) {
		this.userContactsFacade = userContactsFacade;
	}

	@Autowired
	public void setUserSessionFacade(@Qualifier("userSessionFacadeBiz") UserSessionFacade userSessionFacade) {
		this.userSessionFacade = userSessionFacade;
	}

	@Autowired
	public void setCodesFacade(CodesFacade codesFacade) {
		this.codesFacade = codesFacade;
	}

	@Autowired
	public void setEmailFacade(EmailFacade emailFacade) {
		this.emailFacade = emailFacade;
	}

	@Autowired
	public void setPdfParserHelper(PdfParserHelper pdfParserHelper) {
		this.pdfParserHelper = pdfParserHelper;
	}

	@Autowired
	public void setUserFacadeBiz(UserFacade userFacadeBiz) {
		this.userFacadeBiz = userFacadeBiz;
	}

	public static void logExceptionToErrorsTableInDB(Throwable th) {
		Log4jDBUtil.logExceptionToErrorsTableInDB("Gen3Sell - Upload", th);
	}
	
	public ContentContext getContextContext(RequestUserContext requestUserContext, String flowName, String pageName, String profileGroupName) {
		ContentContext contentContext = new ContentContext();
		contentContext.setCobrandId(StubhubCredentials.getRequestContext().getCobrandId());
		contentContext.setCobrand(StubhubCredentials.getRequestContext().getCobrand());
		contentContext.setFlowName(flowName);
		contentContext.setPageName(pageName);
		contentContext.setProfileGroupName(profileGroupName);
		
		if(StubhubCredentials.getRequestContext().getUserGuid() != null) {
			Long userId = userFacadeBiz.getCachedUserIdByGuid(StubhubCredentials.getRequestContext().getUserGuid());
			contentContext.setUserId(userId);
			if (StubhubCredentials.getRequestContext().getUserGuid() != null) {
				try {
					UserGuid userGuid = new UserGuid(StubhubCredentials.getRequestContext().getUserGuid());
					UserContact defaultContact = userContactsFacade.getDefaultContact(userGuid);
					if(defaultContact != null) {
						contentContext.setFirstName(defaultContact.getFirstName());
						contentContext.setLastName(defaultContact.getLastName());
					}
				} catch(Exception e) {
					log.warn("Exception while fetching user default contact ", e);
					DeliveryProcessImpl.logExceptionToErrorsTableInDB(e);
				}
			}
		}
		contentContext.setDomainUrl(DomainUtil.getHttpsWwwDomainUrl());

		return contentContext;
	}
	
	public Content getContent(RequestUserContext requestUserContext, ContentContext contentContext) throws Exception {
		Content content = new Content();
		try {
			com.stubhub.content.business.entity.Content bizContent = null;
			
			// invoke sellContact facade to fetch content by profile group
			bizContent = contentFacade.getContentByProfileGroup(createContentRequestDocument(contentContext), 
																contentContext.getProfileGroupName(),
																contentContext.getCobrandId());
			
			if(bizContent != null && bizContent.getContentMap() != null) {
				content.setContentMap(bizContent.getContentMap());
				for(String key : bizContent.getContentMap().keySet()) {
					log.debug("ContentType : " + key + " AND Value : " + bizContent.getContentMap().get(key));
				}
			}
			
			requestUserContext.getUser().setContent(content);
		} catch (Exception e) {
			log.error("error while getting content", e);
			DeliveryProcessImpl.logExceptionToErrorsTableInDB(e);
		}
		return content;
	}
	
	public Document createContentRequestDocument(ContentContext contentContext) {
		// context dom
		Element context = new Element("context");
		Element base = new Element("default");

		// env element
		base.appendChild(ContentDocumentUtil.createEnvElement());

		// request element
		Element req = new Element("request");
		Element requestProtocol = new Element("requestProtocol");
		requestProtocol.appendChild("https");
		req.appendChild(requestProtocol);
		base.appendChild(req);
		
		Element sellApp = new Element("sellApp");
		Element pageName = new Element("pageName");
		pageName.appendChild(""+StringUtils.trimToEmpty(contentContext.getPageName()));
		sellApp.appendChild(pageName);
		Element flowName = new Element("flowName");
		flowName.appendChild(""+StringUtils.trimToEmpty(contentContext.getFlowName()));
		sellApp.appendChild(flowName);
		// set sellApp element to base
		base.appendChild(sellApp);
		
		Element eventId = new Element("eventId");
		eventId.appendChild(contentContext.getEventId()!=null?String.valueOf(contentContext.getEventId()):"");
		base.appendChild(eventId);

		Element eventCity = new Element("eventCity");
		eventCity.appendChild(StringUtils.trimToEmpty(contentContext.getEventCity()));
		base.appendChild(eventCity);
		Element eventState = new Element("eventState");
		eventState.appendChild(StringUtils.trimToEmpty(contentContext.getEventState()));
		base.appendChild(eventState);
		
		Element genreId = new Element("genreId");
		genreId.appendChild(contentContext.getGenreId()!=null?String.valueOf(contentContext.getGenreId()):"");
		base.appendChild(genreId);

		Element venueConfigId = new Element("venueConfigId");
		venueConfigId.appendChild(contentContext.getVenueConfigId()!=null?String.valueOf(contentContext.getVenueConfigId()):"");
		base.appendChild(venueConfigId);
		
		Element isStateASLRestricted = new Element("stateASLRestricted");
		isStateASLRestricted.appendChild(""+contentContext.isStateASLRestricted());
		base.appendChild(isStateASLRestricted);
		
		Element isCityASLRestricted = new Element("cityASLRestricted");
		isCityASLRestricted.appendChild(""+contentContext.isCityASLRestricted());
		base.appendChild(isCityASLRestricted);
		
		Element isStateTaxable = new Element("stateTaxable");
		isStateTaxable.appendChild(""+contentContext.isStateTaxable());
		base.appendChild(isStateTaxable);
		
		Element isCityTaxable = new Element("cityTaxable");
		isStateASLRestricted.appendChild(""+contentContext.isCityTaxable());
		base.appendChild(isCityTaxable);
		
		Element isLMS = new Element("isLMS");
		isLMS.appendChild(""+contentContext.isLMS());
		base.appendChild(isLMS);
		
		Element isSTH = new Element("isSTH");
		isSTH.appendChild(""+contentContext.isSTH());
		base.appendChild(isSTH);
		
		Element isSeasonTicketPackage = new Element("isSeasonTicketPackage");
		isSeasonTicketPackage.appendChild(""+contentContext.isSeasonTicketPackage());
		base.appendChild(isSeasonTicketPackage);
		
		Element isIntegratedEvent = new Element("isIntegratedEvent");
		isIntegratedEvent.appendChild(""+contentContext.isIntegratedEvent());
		base.appendChild(isIntegratedEvent);

		Element firstName = new Element("firstName");
		firstName.appendChild(StringUtils.trimToEmpty(contentContext.getFirstName()));
		base.appendChild(firstName);
		Element lastName = new Element("lastName");
		lastName.appendChild(StringUtils.trimToEmpty(contentContext.getLastName()));
		base.appendChild(lastName);
		Element userId = new Element("userId");
		userId.appendChild(contentContext.getUserId()!=null?String.valueOf(contentContext.getUserId()):"");
		base.appendChild(userId);
			
		Element deliveryMethod = new Element("deliveryMethod");
		deliveryMethod.appendChild(""+StringUtils.trimToEmpty(contentContext.getDeliveryMethod()));
		base.appendChild(deliveryMethod);
		
		Element domainUrl = new Element("domainUrl");
		domainUrl.appendChild(""+StringUtils.trimToEmpty(contentContext.getDomainUrl()));
		base.appendChild(domainUrl);
		
		Long cobrandId = contentContext.getCobrandId();
		Element cobrand = new Element("cobrandId");
		cobrand.appendChild(cobrandId!=null?String.valueOf(cobrandId):"47");
		base.appendChild(cobrand);
		
		Element ustat = new Element("ustat");
		ustat.appendChild(getUserStatus());
		base.appendChild(ustat);
		
		//add locale elements
		BaseContextDocumentUtil.addLocaleElements(base);
		// set base to context
		context.appendChild(base);
		Document contextDocument = new Document(context);
		
		return contextDocument;
	}
	
	private String getUserStatus() {
		RequestContext reqContext = StubhubCredentials.getRequestContext();
		boolean loggedIn = false;
		boolean guestUser = false;
		String ustat = "";
		try {
			loggedIn = userSessionFacade.validateAuthenticatedSession();
			String userGuid = reqContext.getUserGuid();
			if (userGuid == null) {
				guestUser = true;
			}
			
			if (guestUser) {
				ustat = "guestUser";
			} else if (loggedIn) {
				ustat = "loggedin";

			} else {
				ustat =  "loggedout";
			}

			log.debug("getUserStatus in DeliveryProcessImpl ..." + ustat);

		} catch (Exception e) {

		}
		
		return ustat;
	}
	
			
	public void initTicketFileListForListing(Listing listing) throws Exception {
		if(listing == null) {
			return;
		}
		if(((SplitsSelectionType.AllowInMultiplesOf.equals(listing.getSplitsSelectionType()) || SplitsSelectionType.AnyQuantity.equals(listing.getSplitsSelectionType()))
				&& PDFSplitterSelectionType.WithoutSplitterNoSplit.equals(listing.getPdfSplitterSelectionType())) 
			|| (SplitsSelectionType.None.equals(listing.getSplitsSelectionType()) 
				&& PDFSplitterSelectionType.WithoutSplitter.equals(listing.getPdfSplitterSelectionType())) 
				|| listing.getSplitsSelectionType() == null) {
			listing.setPdfSplitterSelectionType(PDFSplitterSelectionType.NotSelected);
			listing.setTicketFileList(null);
		}
		
		if(PDFSplitterSelectionType.NotSelected.equals(listing.getPdfSplitterSelectionType())) {	
			if((SplitsSelectionType.AllowInMultiplesOf.equals(listing.getSplitsSelectionType()) || SplitsSelectionType.AnyQuantity.equals(listing.getSplitsSelectionType())) && listing.getTicketFileList() == null) {
				createTixFileList(listing);
				createFileInfo(listing);
			} else if(SplitsSelectionType.None.equals(listing.getSplitsSelectionType())) {
			}
		} else if(PDFSplitterSelectionType.WithSplitter.equals(listing.getPdfSplitterSelectionType())) {
			// read from tickets_etix_file and ticket_seat_etix_file table
			createTixFileList(listing);
			createFileInfo(listing);
			List<TicketFile> ticFileList = listing.getTicketFileList();
			for(TicketFile tickFile:ticFileList) {
				if(tickFile.getFile() != null) {
					com.stubhub.inventory.business.entity.TicketFile bizTickFile = 
							new com.stubhub.inventory.business.entity.TicketFile();
					com.stubhub.common.business.entity.StubHubFile bizStubFile = new
							com.stubhub.common.business.entity.StubHubFile();
					bizStubFile.setFilePath(tickFile.getPath());
					bizStubFile.setFileName(tickFile.getDisplayName());
					bizTickFile.setListingId(tickFile.getListingId());
					bizTickFile.setEventId(tickFile.getEventId());
					bizTickFile.setStubHubFile(bizStubFile);
					List<String> imgNames = ticketFileFacade.convertPdf2Png(bizTickFile, 20);
					if((imgNames != null) && !imgNames.isEmpty() && (imgNames.get(0) != null)) {
						tickFile.setImage(imgNames.get(0));
					}
				}
			}
		} else if(PDFSplitterSelectionType.WithoutSplitter.equals(listing.getPdfSplitterSelectionType())) { // should be inflow edit
			boolean startover = false;
			
			if(listing.getTicketFileList() == null) { // just in case	
				startover = true;
			} else { // edit 
				if(listing.getTicketFileList().size() != listing.getQuantity() + (listing.getParkingPassQuantity() != null ? listing.getParkingPassQuantity() : 0)) { // quantity changed
					startover = true;
				} else { // quantity not changed, but seats/parking pass may changed
					Set<Long> source = new HashSet<Long>();
					Set<Long> dest = new HashSet<Long>();
					
					for(TicketSeat ts: listing.getTicketSeats()) {
						if(ts.getDbTicketSeatId() != null) {
							source.add(ts.getDbTicketSeatId());
						}
					}
					if(listing.getParkingPassQuantity() != null && listing.getParkingPassQuantity() > 0) {
						for(TicketSeat pp: listing.getParkingPasses()) {
							if(pp.getDbTicketSeatId() != null) {
								source.add(pp.getDbTicketSeatId());
							}
						}
					}
					
					for(TicketFile tf: listing.getTicketFileList()) {
						dest.add(tf.getTicketSeatId());
					}
					
					if(!(source.containsAll(dest) && dest.containsAll(source))) {
						startover = true;
					} 
				}
			}
			if(startover) { //re-build the list and get existing files
				createTixFileList(listing);
				createFileInfo(listing);
			}
		}
	}
	
	/**
	 * populate file info to each TicketFile instance
	 * @param listing
	 */
	private void createFileInfo(Listing listing) {
		List<TicketFile> tickFiles = listing.getTicketFileList();
		List<Long> ticketSeatIds =  new ArrayList<Long>();
		if(tickFiles == null) {
			return;
		}
		for(TicketFile temp:tickFiles) {
			if((temp != null)&&(temp.getTicketSeatId() != null)) {
				ticketSeatIds.add(temp.getTicketSeatId());
			}
		}
		Map<Long, com.stubhub.inventory.business.entity.TicketFile> result = ticketFileFacade.getTicketFilesForSeats(listing.getId(), ticketSeatIds);
		for(TicketFile temp:tickFiles) {
			if((temp != null)&&(temp.getTicketSeatId() != null)) {
				com.stubhub.inventory.business.entity.TicketFile bizTicFile = result.get(temp.getTicketSeatId());
				
				if(bizTicFile != null) {
					com.stubhub.common.business.entity.StubHubFile bizStubFile = 
							bizTicFile.getStubHubFile();
					if(bizStubFile.getDisplayName() != null) {
						temp.setDisplayName(bizStubFile.getDisplayName());
					}
					if(bizStubFile.getFileId() != null) {
						temp.setFileId(bizStubFile.getFileId());
					}
					if(bizStubFile.getOriginalFileName() != null) {
						temp.setOriginalFileName(bizStubFile.getOriginalFileName());
					}
					if(bizStubFile.getFileSize() != null) {
						temp.setFileSize(bizStubFile.getFileSize());
					}
					if(bizStubFile.getFilePath() != null) {
						temp.setPath(bizStubFile.getFilePath());
					}
					
					temp.setDirty(false);
				}
			}
		}
	}
	
	
	//populate ticket File list to one listing
	private void createTixFileList(Listing listing) {
		// split, prepare ticketFile for each seat, pp included
		List<TicketFile> tixFileList = new ArrayList<TicketFile>();
		
		boolean isPiggyback = listing.getPiggyback();
						
		for(TicketSeat ts: listing.getTicketSeats()) {

			if(ts.getDbTicketSeatId() != null) {
				TicketFile tf = new TicketFile();
				tf.setEventId(listing.getEvent().getId());
				tf.setListingId(listing.getId());
				tf.setParkingPass(false);
				tf.setRow(listing.getRow());
				String seat = ts.getSeatNum();
				if(StringUtils.isBlank(seat)) {
					seat = "N/A";
				}
				tf.setSeat(seat);
				tf.setTicketSeatId(ts.getDbTicketSeatId());
				tf.setPiggyback(isPiggyback);
				tf.setOriginal(false);
				tf.setUploadSuccessful(true);

				tixFileList.add(tf);
			}
		}
		
		if(isPiggyback) {
			for(TicketSeat ts: listing.getPiggyBackTicketSeats()) {

				if(ts.getDbTicketSeatId() != null) {
					TicketFile tf = new TicketFile();
					tf.setEventId(listing.getEvent().getId());
					tf.setListingId(listing.getId());
					tf.setParkingPass(false);
					tf.setRow(listing.getRow());
					tf.setSeat(ts.getSeatNum());
					tf.setTicketSeatId(ts.getDbTicketSeatId());
					tf.setPiggyback(isPiggyback);
					tf.setOriginal(false);

					tixFileList.add(tf);
				}
			}
		}
		
		for(TicketSeat ts: listing.getParkingPasses()) {

			if(ts.getDbTicketSeatId() != null) {
				TicketFile tf = new TicketFile();
				tf.setEventId(listing.getEvent().getId());
				tf.setListingId(listing.getId());
				tf.setParkingPass(true);					
				tf.setTicketSeatId(ts.getDbTicketSeatId());
				tf.setPiggyback(isPiggyback);
				tf.setOriginal(false);

				tixFileList.add(tf);
			}
		}
		
		listing.setTicketFileList(tixFileList);
		
	}

    /**
     * @see com.stubhub.ui.business.process.DeliveryProcess#uploadTicketFile(com.stubhub.ui.model.TicketFile,
     *      org.apache.tapestry5.upload.services.UploadedFile)
     */
    public UploadFileInfo uploadTicketFile(TicketFile ticketFile, UploadedFile uploadedFile) throws Exception {

        String originalFileName = uploadedFile.getFileName();
		
        /* Added for Ticket Protection - Start */
        UploadFileInfo uploadFileInfo = this.ticketFileFacade.getUploadedFilePath(ticketFile.getListingId(),
                                                                                  ticketFile.getEventId(), originalFileName,
                                                                                  uploadedFile.getStream(), false);

        if (null != uploadFileInfo) {
            String uploadedFilePath = uploadFileInfo.getUploadedFilePath();
            log.debug("Uploaded File Path : " + uploadFileInfo.getUploadedFilePath());
            String displayName = null;
            if (!StringUtils.isStringNullorEmpty(uploadedFilePath)) {
                displayName = uploadedFilePath;
                int fileNamePos = uploadedFilePath.lastIndexOf("/");
                if (fileNamePos == -1) {
                    fileNamePos = uploadedFilePath.lastIndexOf("\\");                    
			}
                displayName = uploadedFilePath.substring(fileNamePos + 1);
                ticketFile.setPath(uploadedFilePath.substring(0, fileNamePos));
                LogSF.debug(log, "originalFileName={}", originalFileName);
                ticketFile.setDisplayName(displayName);
		}
            /* Added for Ticket Protection - End */
            ticketFile.setOriginalFileName(originalFileName);

            /* Added for Ticket Protection - Start */
            ticketFile.setFileSize(uploadedFile.getSize());
            /* Added for Ticket Protection - End */
		ticketFile.setOriginal(true);
		ticketFile.setFileId(System.currentTimeMillis());
		ticketFile.setDirty(true);
            ticketFile.setUploadSuccessful(uploadFileInfo.isUploadSuccessful());
            ticketFile.setPageCount(uploadFileInfo.getPageCount());
        } else {
            Object[] loggerArgumentArray = new Object[] { ticketFile.getListingId() };
            LogSF.error(log, "Path returned by uploadFile service is NULL for listingId={}", loggerArgumentArray);
            throw new StubHubFileException("Unable to create file in the nas repository", StubHubFileException.INVALID_PDF_FILE);
        }
        return uploadFileInfo;
	}

	public void validatePdfFile(TicketFile ticketFile)
			throws StubHubFileException {
		ticketFileFacade.validatePdfFileForUpload(ticketFile.getFile());
	}
	
	public int getPageCount(TicketFile ticketFile) throws Exception {
	    int pageCount = ticketFile.getPageCount();
            LogSF.debug(log, "pageCount={}", pageCount);
            return pageCount ;
	}

	public void updatePdfUploadType(Listing listing) {
		if(PDFSplitterSelectionType.NotSelected.equals(listing.getPdfSplitterSelectionType())) {
			List<Long> seatIds = new ArrayList<Long>();
			for(TicketSeat tic: listing.getTicketSeats()) {
				if(tic.getDbTicketSeatId() != null) {
					seatIds.add(tic.getDbTicketSeatId());
				}
			}
			if(listing.getParkingPasses() != null && !listing.getParkingPasses().isEmpty()) {
				for(TicketSeat pp: listing.getParkingPasses()) {
					if(pp.getDbTicketSeatId() != null) {
						seatIds.add(pp.getDbTicketSeatId());
					}
				}
			}
			
			if(ticketFileFacade.isSplitted(listing.getId(), seatIds)) {
				listing.setPdfSplitterSelectionType(PDFSplitterSelectionType.WithSplitter);
			} else {
				List<TicketFile> tixFiles = listing.getTicketFileList();
				if(tixFiles != null && !tixFiles.isEmpty()) {
					boolean isSplit = false;
					boolean hasUploadedFile = false;
					for(TicketFile tf: tixFiles) {
						if(tf.getFile() != null) {
							hasUploadedFile = true;
						}
						
						if(tf.getTicketSeatId() != null) {
							isSplit = true;							
						} 					
					}
					
					if(hasUploadedFile) {
						if(isSplit) {
							listing.setPdfSplitterSelectionType(PDFSplitterSelectionType.WithoutSplitter);
						} else {
							listing.setPdfSplitterSelectionType(PDFSplitterSelectionType.WithoutSplitterNoSplit);
						}
					} 
				}
			}
		}
	}
	
	public List<com.stubhub.inventory.business.entity.TicketFile> saveTicketFilesForListing(Listing listing) throws Exception {
	    List<com.stubhub.inventory.business.entity.TicketFile> ticketFileList = new ArrayList<com.stubhub.inventory.business.entity.TicketFile>();
	    if(PDFSplitterSelectionType.WithoutSplitter.equals(listing.getPdfSplitterSelectionType()) ||
				PDFSplitterSelectionType.WithoutSplitterNoSplit.equals(listing.getPdfSplitterSelectionType())) {
			List<com.stubhub.inventory.business.entity.TicketFile> passedInTickFiles = 
					new ArrayList<com.stubhub.inventory.business.entity.TicketFile>();
			LogSF.debug(log,"Invoking saveTicketFilesForListing  with listingId={}", listing.getId());
			List<com.stubhub.ui.model.upload.TicketFile> uiTicketFiles = null;
            uiTicketFiles = listing.getTicketFileList();
            if(null != uiTicketFiles){
			for(com.stubhub.ui.model.upload.TicketFile uiTicTemp:uiTicketFiles) {
				if(uiTicTemp.isDirty()) {
					com.stubhub.inventory.business.entity.TicketFile bizTickFile = 
							new com.stubhub.inventory.business.entity.TicketFile();
					StubHubFile bizStub = new StubHubFile();
    					
					bizStub.setFilePath(uiTicTemp.getPath());
					bizStub.setFileName(uiTicTemp.getDisplayName());
					bizStub.setOriginalFileName(uiTicTemp.getOriginalFileName());
					bizStub.setFileSize(uiTicTemp.getFileSize());
					bizStub.setDisplayName(uiTicTemp.getDisplayName());

                    LogSF.debug(log, "Input displayFileName={} originalFileName={}", uiTicTemp.getDisplayName(), uiTicTemp.getOriginalFileName());
					bizTickFile.setStubHubFile(bizStub);
					bizTickFile.setListingId(listing.getId());
					bizTickFile.setEventId(listing.getEvent().getId());
					bizTickFile.setActive(true);
					bizTickFile.setSeats(uiTicTemp.getSeat());
					bizTickFile.setCreatedBy("Gen3 Sell");
					bizTickFile.setLastUpdatedBy("Gen3 Sell");
					if(uiTicTemp.getTicketSeatId() != null) {
						com.stubhub.inventory.business.entity.TicketSeatFile bizTicSeatFile = 
								new com.stubhub.inventory.business.entity.TicketSeatFile();
						bizTicSeatFile.setTicketSeatId(uiTicTemp.getTicketSeatId());
						bizTickFile.setTicketSeatFile(bizTicSeatFile);
					}
					
					passedInTickFiles.add(bizTickFile);
					LogSF.debug(log,"Added ticketFile to listingId={}", bizTickFile.getListingId());
    				uiTicTemp.setDirty(false);
    				LogSF.debug(log, "Stububfile displayFileName={} originalFileName={} fileName={}", 
    					                           bizStub.getFilePath(), bizStub.getOriginalFileName(), bizStub.getFileName());
    				}
				}
			}
			ticketFileFacade.saveStubHubFiles(passedInTickFiles,false);
			ticketFileList= ticketFileFacade.addListingTicketFiles(passedInTickFiles);
		}
	    return ticketFileList;
	}
	
	public boolean getPdfSplitterEnabled() {
		return StubHubProperties.getPropertyAsBoolean("pdfSplitter.enabled", true);
	}
	
	public String getDomain() {
		return DomainUtil.getStubhubDomain();
	}

	public Listing getListing(Long listingId, List<Long> seatIdList) {
		Listing uiListing = null;
		com.stubhub.catalog.business.entity.Event bizEvent = null;
		
		log.debug("getListingById....START.. listingId " + listingId);
		try{
			if(listingId == null){
				return null;				
			}		 	
			com.stubhub.inventory.business.entity.Listing bizListing =  sfrListingProcessFacade.getFullListing(listingId);				
			if(bizListing!=null){					
				
				List<Ticket> incompleteTickets = sfrListingProcessFacade.findIncompleteTickets(listingId);
				
				log.debug("Populating the ui tickets");
				uiListing = new Listing();
				if (bizListing.getLstTickets() != null) {
					if(incompleteTickets != null) {
						bizListing.getLstTickets().addAll(incompleteTickets);
					}
					uiListing = this.convertBizTicketsToUiTicketSeats(bizListing.getLstTickets(), uiListing, seatIdList);
				}
				long ticketQuantity = 0;
				long parkingPassQuantity = 0;
				if(uiListing.getTicketSeats() != null) {
					for(TicketSeat ts: uiListing.getTicketSeats()) {
						if(ts.getDbTicketSeatId() != null) {
							ticketQuantity++;
						}
					}
				}
//				if(bizListing.getTicketSeats() != null) {
//					ticketQuantity = bizListing.getTicketSeats().size();
//				}
//				if(bizListing.getParkingPasses() != null) {
//					parkingPassQuantity = bizListing.getParkingPasses().size();
//				}
				if(uiListing.getParkingPasses() != null) {
					for(TicketSeat ts: uiListing.getParkingPasses()) {
						if(ts.getDbTicketSeatId() != null) {
							parkingPassQuantity++;
						}
					}
				}
				if (ticketQuantity > 0) {
					log.debug("Ticket Quantity" + ticketQuantity);
					uiListing.setQuantity(ticketQuantity);
				}
				if (parkingPassQuantity > 0) {
					log.debug("Parking pass quanity" + parkingPassQuantity);
					uiListing.setParkingPassQuantity(parkingPassQuantity);
				}
				
				log.debug("Populating the Seat Type");
				if(bizListing.getLstTickets().get(0).getIsGeneralAdmission()){
					log.debug("Listing has General Admission Tickets");
					uiListing.setSeatType(SeatType.GeneralAdmission);						
				}else{
					log.debug("Listing has Specific Tickets");
					uiListing.setSeatType(SeatType.SpecificSeats);	
				}
								
				uiListing.setId(bizListing.getId());
				log.debug("Populating the uiEvents");
				// since event information as part of this call is not retrieving venue/genre information
				Long eventId = bizListing.getEvent().getId();
				bizEvent = eventFacade.getEventWithMeta(eventId);				
			    com.stubhub.ui.model.upload.Event  uiEvent = this.convertBizEventToUIEvent(bizEvent);
				
				uiListing.setEvent(uiEvent);
				
			    SellOptions sellOption = bizListing.getSellOptions();			    
			    SplitsSelectionType splitsSelectionType = SplitsSelectionType.None;
				if(sellOption.getSplitOption() == 1) {
					log.debug("Seller selected AllowInMultiplesOf split option for ListingId: " + bizListing.getId());
					splitsSelectionType = SplitsSelectionType.AllowInMultiplesOf;
				} else if(sellOption.getSplitOption() == 2) {
					log.debug("Seller selected AnyQuantity split option for ListingId: " + bizListing.getId());
					splitsSelectionType = SplitsSelectionType.AnyQuantity;
				} else {
					log.debug("Seller did not select the split option for ListingId: " + bizListing.getId());
				}
				uiListing.setSplitsSelectionType(splitsSelectionType);
			}
			
	    }
	    catch(Exception e){
	    	 log.error("Exception occured while getting Listing by Id",e);
	    	 DeliveryProcessImpl.logExceptionToErrorsTableInDB(e);
	    }
	    
	    log.debug("getListingById....FINISH.. listingId " + listingId);
		return uiListing;
	}
	
	private Listing convertBizTicketsToUiTicketSeats(List<Ticket> lstTickets, Listing uiListing, List<Long> seatIdList) {		
		List<String> lstUniqueRows = new ArrayList<String>();
		List<Ticket> bizLstTickets = new ArrayList<Ticket>(); 
		List<Ticket> piggyBackTickets = new ArrayList<Ticket>(); 
		List<TicketSeat> uiTicketSeats = new ArrayList<TicketSeat>();
		List<TicketSeat> uiPiggyBackSeats = new ArrayList<TicketSeat>();
		List<TicketSeat> uiParkingPasses  = new ArrayList<TicketSeat>();		
		
		log.debug("Populating the Ticket Seats");  
		
		/*Populating the uiTicket Seats*/
		for (long i = 1; i <= 150; i++) {
			TicketSeat ticketSeat = new TicketSeat();
			ticketSeat.setSeatId(i);
			uiTicketSeats.add(ticketSeat);
		}
		for (long i = 1; i <= 75; i++) {
			TicketSeat ticketSeat = new TicketSeat();
			ticketSeat.setSeatId(i);
			uiPiggyBackSeats.add(ticketSeat);
		}
	  /* Checking for unique rows*/	
		for (Ticket bizTicket : lstTickets) {
			if (bizTicket.isRegularTicket()) {
				if(seatIdList.contains(bizTicket.getId())) {
					com.stubhub.inventory.business.entity.TicketSeat ticketSeat = (com.stubhub.inventory.business.entity.TicketSeat)bizTicket;
					if (!lstUniqueRows.contains(ticketSeat.getRow())) {
						log.debug("List of Unique rows: "+ticketSeat.getRow());
						lstUniqueRows.add(ticketSeat.getRow());					
					}
				}
			}
		}		
		log.debug("Populating the Ui Ticket Seats");		
        /* Populate non-piggyBack back tickets*/
		if (lstUniqueRows.size() == 1) {					
			uiListing.setPiggyback(false);
			log.debug("Populating the non piggy back tickets");
			for (int i = 0; i < lstTickets.size(); i++) {
				com.stubhub.inventory.business.entity.Ticket bizTicket = lstTickets.get(i);
				if(seatIdList.contains(bizTicket.getId())) {
					if (bizTicket.isRegularTicket()) {					
						com.stubhub.inventory.business.entity.TicketSeat bizTicketSeat = (com.stubhub.inventory.business.entity.TicketSeat)bizTicket;
						log.debug("Ticket seat is a regular ticket "+bizTicketSeat.getId()+", " +bizTicketSeat.getRow());
						TicketSeat uiTicketSeat = uiTicketSeats.get(i);
						uiTicketSeat.setSeatNum(bizTicketSeat.getSeatNumber());
						uiTicketSeat.setDbTicketSeatId(bizTicketSeat.getId());
						uiListing.setSection(bizTicketSeat.getSection());
						uiListing.setRow(bizTicketSeat.getRow());							
						
					} else if (bizTicket.isParkingPass()){
						com.stubhub.inventory.business.entity.ParkingPass parkingPass = (com.stubhub.inventory.business.entity.ParkingPass)bizTicket;
						log.debug("Ticket seat is a Parking Pass "+parkingPass.getId()+", " +parkingPass.getRow());
						TicketSeat uiParkingPass = new TicketSeat();
						uiParkingPass.setSystemSectionName(parkingPass.getLot());
						uiParkingPass.setSeatNum(parkingPass.getStall());
					 	if(parkingPass.getId() != null) {
						   uiParkingPass.setDbTicketSeatId(parkingPass.getId());
						}	
						uiParkingPasses.add(uiParkingPass);
					}
				}
			}		
		}
		 /* Populate piggyBack back tickets*/
		if (lstUniqueRows.size() > 1) {
			uiListing.setPiggyback(true);
			log.debug("Populating the piggyBack ticket seats.");
			for (Ticket bizTicket : lstTickets) {
				if(seatIdList.contains(bizTicket.getId())) {
					log.debug("List of Ticket Seats: " +lstTickets.size());
					if (bizTicket.isRegularTicket()) {	
						com.stubhub.inventory.business.entity.TicketSeat bizTicketSeat = (com.stubhub.inventory.business.entity.TicketSeat)bizTicket;
						if (lstUniqueRows.get(0).toString().equals(
								bizTicketSeat.getRow())) {
							bizLstTickets.add(bizTicket);						
						} else if (lstUniqueRows.get(1).toString().equals(
								bizTicketSeat.getRow())) {
							piggyBackTickets.add(bizTicket);
						}
					} else if (bizTicket.isParkingPass()) {
						com.stubhub.inventory.business.entity.ParkingPass parkingPass = (com.stubhub.inventory.business.entity.ParkingPass) bizTicket;
						TicketSeat uiParkingPass = new TicketSeat();					
						uiParkingPass.setSystemSectionName(parkingPass.getLot());
						uiParkingPass.setSeatNum(parkingPass.getStall());				
						if(parkingPass.getId() != null){
							uiParkingPass.setDbTicketSeatId(parkingPass.getId());
						}	
						uiParkingPasses.add(uiParkingPass);
					}
				}
			}
			for (int i = 0; i < bizLstTickets.size(); i++) {				
				log.debug("List of First Row Seats"+bizLstTickets.size());
				com.stubhub.inventory.business.entity.Ticket bizTicket = bizLstTickets
						.get(i);
				if(seatIdList.contains(bizTicket.getId())) {
	    			com.stubhub.inventory.business.entity.TicketSeat bizTicketSeat = (com.stubhub.inventory.business.entity.TicketSeat)bizTicket;
	    			log.debug("Ticket seat is a regular ticket "+bizTicketSeat.getId()+", " +bizTicketSeat.getRow());
					TicketSeat uiTicketSeat = uiTicketSeats.get(i);
					uiTicketSeat.setSeatNum(bizTicketSeat.getSeatNumber());
					uiTicketSeat.setDbTicketSeatId(bizTicketSeat.getId());
					uiListing.setSection(bizTicketSeat.getSection());
					uiListing.setRow(bizTicketSeat.getRow());	
				}
			}
			for (int i = 0; i < piggyBackTickets.size(); i++) {
				com.stubhub.inventory.business.entity.Ticket piggyBackTicket = piggyBackTickets
						.get(i);
				if(seatIdList.contains(piggyBackTicket.getId())) {
					com.stubhub.inventory.business.entity.TicketSeat bizTicketSeat = (com.stubhub.inventory.business.entity.TicketSeat)piggyBackTicket;
					log.debug("Ticket seat is a piggyBackTicket Seat "+bizTicketSeat.getId()+", " +bizTicketSeat.getRow());
					TicketSeat uiPiggyBackSeat = uiPiggyBackSeats.get(i);
					uiListing.setPiggyback(true);
					uiPiggyBackSeat.setSeatNum(bizTicketSeat.getSeatNumber());
					uiPiggyBackSeat.setDbTicketSeatId(piggyBackTicket.getId());
					uiListing.setSection(bizTicketSeat.getSection());
					uiListing.setPiggyBackRow(bizTicketSeat.getRow());	
				}
			} 
		}		
		uiListing.setTicketSeats(uiTicketSeats);
		uiListing.setPiggyBackTicketSeats(uiPiggyBackSeats);
		uiListing.setParkingPasses(uiParkingPasses);		
		return uiListing;
	}
	
	
	
	private com.stubhub.ui.model.upload.Event convertBizEventToUIEvent(com.stubhub.catalog.business.entity.Event bizEvent) {		
		com.stubhub.ui.model.upload.Event uiEvent = new com.stubhub.ui.model.upload.Event();
		uiEvent.setId(bizEvent.getId());
		uiEvent.setDescription(bizEvent.getDescription());
		uiEvent.setEventDate(bizEvent.getEventDate());
		/*TimeZone tz = bizEvent.getVenue().getTimeZone();
		if (tz != null) {
			GregorianCalendar eventLocalTimeZone = new GregorianCalendar(tz);
			eventLocalTimeZone.setTimeInMillis(bizEvent.getLocalEventDate().getTimeInMillis());
			uiEvent.setLocalEventDate(eventLocalTimeZone);
		} else {
			uiEvent.setLocalEventDate(bizEvent.getLocalEventDate());
		}*/
		TimeZone timeZone = bizEvent.getVenue().getTimeZone();
		if (bizEvent.getVenue().getZipInfo() != null) {
			String timeZoneId = bizEvent.getVenue().getZipInfo().getTimeZone();
			if (timeZone != null) {
				timeZoneId = timeZone.getID();
			}
			// set event local time
			Calendar eventLocal = NewDateUtils.getEventLocalDate(timeZoneId, bizEvent.getVenue().getZipInfo()
					.isDaylightSaving(), bizEvent.getEventDate(), bizEvent.getLocalEventDate());
			uiEvent.setLocalEventDate(eventLocal);
		}

		log.debug("SellProcessAdapter .. venueConfigId " + bizEvent.getVenueConfigId() + " for eventId " + bizEvent.getId());
		com.stubhub.ui.model.Venue uiVenue = new com.stubhub.ui.model.Venue();
		uiVenue.setDescription(bizEvent.getVenue().getGeo().getDescription());
		uiVenue.setId(bizEvent.getVenueConfigId());
		uiVenue.setConfigId(bizEvent.getVenueConfigId());
		uiVenue.setCity(bizEvent.getVenue().getCity());
		String state = bizEvent.getVenue().getState();
		if (StringUtils.trimToEmpty(state).length() > 1) {
			uiVenue.setState(state);
		}else{
			uiVenue.setState("");
		}
		String countryCode = bizEvent.getVenue().getCountry();
		if (countryCode != null && !CommonUtil.isUSOrCACountry(countryCode)) {
			StubCountries country = codesFacade.getCountryByCode(countryCode);
			if (country != null) {
				uiVenue.setCountry(CommonUtil.getCountryAbbr(country.getCountryName()));
			} else {
				uiVenue.setCountry(countryCode);
			}
		}
		uiVenue.setVenueTimeZone(timeZone);			
		if(bizEvent.getVenue().getZipInfo() != null) {
			String timeZoneId = bizEvent.getVenue().getZipInfo().getTimeZone();
			if (timeZone != null) {
				timeZoneId = timeZone.getID();
			}else{
				uiVenue.setVenueTimeZone(TimeZone.getTimeZone(timeZoneId));
			}
			uiVenue.setDbTimeZone(timeZoneId);
			uiVenue.setDaylightSaving(bizEvent.getVenue().getZipInfo().isDaylightSaving());
		}
		uiEvent.setVenue(uiVenue);			
		
		return uiEvent;		
	}
	
	/**
	 * This method is used for parsing the pdf files and also performing
	 * validations against the uploaded data.
	 * 
	 * @param ticketFileList
	 *            - List of ticketFiles that are uploaded for this listing..
	 * @param listing
	 *            - Listing instance associated to this validation
	 * 
	 */
	public Set<Long> parsePdfData(List<com.stubhub.inventory.business.entity.TicketFile> ticketFileList, Listing listing) {
		Long eventId = (null == listing.getEvent() ? listing.getEventId() : listing.getEvent().getId());

		List<Long> fileInfoIdList = new ArrayList<Long>();
		for (com.stubhub.inventory.business.entity.TicketFile ticketFile : ticketFileList) {
			fileInfoIdList.add(ticketFile.getStubHubFile().getFileId());
		}

		Set<Long> errorCodes = new HashSet<Long>();
		try {
			errorCodes = pdfParserHelper.parsePDFWithApi(fileInfoIdList, false);
			Long sellerId = userFacadeBiz.getCachedUserIdByGuid(StubhubCredentials.getRequestContext().getUserGuid());
			LogSF.debug(log,"PDF parsing result errorCodes={} eventId={} listingId={} sellerId={}" , errorCodes , eventId.toString()
					, listing.getId().toString() ,String.valueOf(sellerId));

			if (errorCodes.isEmpty()) {
				// add log indicating that it is a valid PDF file
				Object[] argumentArray = { PDFReasonCode.ValidPdfFile.getDescription(), sellerId, listing.getId(),
						fileInfoIdList.toString().replace("[", "").replace("]", "") };
				LogSF.debug(log, "{} websiteUserId={} ticketId={} fileInfoIds={}", argumentArray);
			}
		} catch (JAXBException ex) {
			log.error("JAXBException when parsing non-split PDF, listingId=" + listing.getId(), ex);
		} catch (Exception e) {
			// Catch the possible socket exception because the parse process should not block the sell flow 
			log.error("Exception when parsing non-split PDF, listingId=" + listing.getId(), e);
		}

		return errorCodes;
	}
	
	
	/* (non-Javadoc)
	 * @see com.stubhub.ui.business.process.impl.CommonProcessImpl#updateListingStatus(java.lang.Long)
	 */
	public void updateListingStatus(Long listingId) throws AccessControlException, StubHubBizException{
	     super.updateListingStatus(listingId);
	}
	
	/* (non-Javadoc)
	 * @see com.stubhub.ui.business.process.DeliveryProcess#sendListingDeletedEmail(java.util.List)
	 */
	public void sendPDFListingCreationFailureEmails(List<Long> listingIds) throws InvalidArgumentException, Exception {
	    emailFacade.sendPDFListingCreationFailureEmail(listingIds);
	}

    /* Added for Ticket Protection - Start */

    /**
     * @see com.stubhub.ui.business.process.DeliveryProcess#validatePdfContent(java.io.InputStream)
     */
    public void validatePdfContent(InputStream inputStream) throws StubHubFileException, MissingRequiredArgumentException {
        if (null == inputStream) {
            throw new MissingRequiredArgumentException(Argument.INPUT_STREAM, ErrorConstants.INPUT_STREAM_NULL);
        }
        LogSF.debug(log, "Calling validatePdfContent on the given InputStream at Delivery Process Layer", null);
        ticketFileFacade.validatePdfContent(inputStream);
       
    }

    /**
     * @see com.stubhub.ui.business.process.DeliveryProcess#doVirusScanOnPdfInputStream(java.io.InputStream)
     */
    public void doVirusScanOnPdfInputStream(InputStream inputStream) throws StubHubFileException, MissingRequiredArgumentException {
        if (null == inputStream) {
            throw new MissingRequiredArgumentException(Argument.INPUT_STREAM, ErrorConstants.INPUT_STREAM_NULL);
        }
        LogSF.debug(log, "Before calling doVirusScanOnPdfInputStream, InputStream object is not NULL ", null);
        ticketFileFacade.doVirusScanOnPdfInputStream(inputStream);

    }
   
    /**
     * @see com.stubhub.ui.business.process.DeliveryProcess#validatePdfFileForUpload(java.io.InputStream)
     */
    public void validatePdfFileForUpload(InputStream inputStream) throws MissingRequiredArgumentException, StubHubFileException {
        if (null == inputStream) {
            throw new MissingRequiredArgumentException(Argument.INPUT_STREAM, ErrorConstants.INPUT_STREAM_NULL);
        }
        LogSF.debug(log, "Calling validatePdfFileForUpload on the given InputStream at Ticket Delivery Process Layer", null);
        ticketFileFacade.validatePdfFileForUpload(inputStream);
    }

    /* Added for Ticket Protection - End */ 

}

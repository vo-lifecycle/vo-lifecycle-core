package org.volifecycle.event.vo;

import java.io.Serializable;
import java.util.Calendar;
import java.util.List;
import java.util.Map;

/**
 * Event value object class
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public class Event implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * Type of event (issue, etc)
     */
    protected String typeEvent;

    /**
     * when ?
     */
    protected Calendar date;

    /**
     * what ?
     */
    protected String details;

    /**
     * who ?
     */
    protected String actor;

    /**
     * Value object impacted
     */
    protected String idValueObject;

    /**
     * Type of value object impacted
     */
    protected String typeValueObject;

    /**
     * List of failed action's ids
     */
    protected List<String> failedActionsIds;

    /**
     * Additional informations (like comments, sources, etc)
     */
    protected Map<String, String> additionalInformations;

    /**
     * @return the typeEvent
     */
    public String getTypeEvent() {
        return typeEvent;
    }

    /**
     * @param typeEvent
     *            the typeEvent to set
     */
    public void setTypeEvent(String typeEvent) {
        this.typeEvent = typeEvent;
    }

    /**
     * @return the date
     */
    public Calendar getDate() {
        return date;
    }

    /**
     * @param date
     *            the date to set
     */
    public void setDate(Calendar date) {
        this.date = date;
    }

    /**
     * @return the details
     */
    public String getDetails() {
        return details;
    }

    /**
     * @param details
     *            the details to set
     */
    public void setDetails(String details) {
        this.details = details;
    }

    /**
     * @return the actor
     */
    public String getActor() {
        return actor;
    }

    /**
     * @param actor
     *            the actor to set
     */
    public void setActor(String actor) {
        this.actor = actor;
    }

    /**
     * @return the idValueObject
     */
    public String getIdValueObject() {
        return idValueObject;
    }

    /**
     * @param idValueObject
     *            the idValueObject to set
     */
    public void setIdValueObject(String idValueObject) {
        this.idValueObject = idValueObject;
    }

    /**
     * @return the typeValueObject
     */
    public String getTypeValueObject() {
        return typeValueObject;
    }

    /**
     * @param typeValueObject
     *            the typeValueObject to set
     */
    public void setTypeValueObject(String typeValueObject) {
        this.typeValueObject = typeValueObject;
    }

    /**
     * @return the additionalInformations
     */
    public Map<String, String> getAdditionalInformations() {
        return additionalInformations;
    }

    /**
     * @param additionalInformations
     *            the additionalInformations to set
     */
    public void setAdditionalInformations(Map<String, String> additionalInformations) {
        this.additionalInformations = additionalInformations;
    }

    /**
     * @return the failedActionsIds
     */
    public List<String> getFailedActionsIds() {
        return failedActionsIds;
    }

    /**
     * @param failedActionsIds
     *            the failedActionsIds to set
     */
    public void setFailedActionsIds(List<String> failedActionsIds) {
        this.failedActionsIds = failedActionsIds;
    }
}

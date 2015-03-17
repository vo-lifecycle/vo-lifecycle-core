package org.volifecycle.event.vo;

import java.util.Calendar;

/**
 * Event value object class
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public class Event {
    /**
     * Type of event (issue, etc)
     */
    private String typeEvent;

    /**
     * when ?
     */
    private Calendar date;

    /**
     * what ?
     */
    private String details;

    /**
     * who ?
     */
    private String actor;

    /**
     * Value object impacted
     */
    private String idValueObject;

    /**
     * Type of value object impacted
     */
    private String typeValueObject;

    /**
     * @return the typeEvent
     */
    public String getTypeEvent() {
        return typeEvent;
    }

    /**
     * @param typeEvent the typeEvent to set
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
     * @param date the date to set
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
     * @param details the details to set
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
     * @param actor the actor to set
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
     * @param idValueObject the idValueObject to set
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
     * @param typeValueObject the typeValueObject to set
     */
    public void setTypeValueObject(String typeValueObject) {
        this.typeValueObject = typeValueObject;
    }
}

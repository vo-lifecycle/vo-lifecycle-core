package org.volifecycle.utils;

import ma.glasnost.orika.MapperFacade;
import ma.glasnost.orika.MapperFactory;
import ma.glasnost.orika.impl.DefaultMapperFactory;

/**
 * A factory for a bean mapper instance.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 */
public class BeanMapperFactory {
  private static final BeanMapperFactory MAPPER = new BeanMapperFactory();

  private MapperFactory orikaMapperFactory;
  private MapperFacade orikaMapperFacade;

  /**
   * Private constructor.
   */
  private BeanMapperFactory() {
    orikaMapperFactory = new DefaultMapperFactory.Builder().build();
    orikaMapperFacade = orikaMapperFactory.getMapperFacade();
  }

  /**
   * Return a single instance of BeanMapperFactory.
   * 
   * @return BeanMapperFactory
   */
  public static BeanMapperFactory getInstance() {
    return MAPPER;
  }

  /**
   * Copy of object in an instance of the destination class.
   * 
   * @param source
   * @param clazz
   * @return T instance
   */
  public <T> T map(Object source, Class<T> clazz) {
    return orikaMapperFacade.map(source, clazz);
  }

  /**
   * Copy of object in an instance in the destination instance.
   * 
   * @param source
   * @param clazz
   * @return T instance
   */
  public void map(Object source, Object dest) {
    orikaMapperFacade.map(source, dest);
  }
}

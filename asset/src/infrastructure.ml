open Entity
open Action_enum

module MInfrastructure = struct
  type infrastructure_type = CITY | FACTORY | AIRPORT | TECHLAB | NUCLEARFOUNDRY

  class infrastructure r q hp ap mp current_mp atks defs ar pa aos faction t= 
    object(self)
      inherit MEntity.entity r q hp ap mp current_mp atks defs ar pa aos faction as super
      val t : infrastructure_type = t
      method get_type = t
    end

  type t = infrastructure

  let create_city r q f =
    new infrastructure r q 300 100 0 0 80 100 2 [MAction_enum.PRODUCE] [] f CITY
end
;;